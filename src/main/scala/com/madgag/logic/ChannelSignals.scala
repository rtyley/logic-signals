package com.madgag.logic

import com.madgag.algo.sorting.kwaymerge.Merge.mergeIterable
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.ChannelSignals.groupAdjacent
import com.madgag.logic.signals.triggers.ChannelGroup
import com.madgag.logic.signals.triggers.Criterion.SignalChange
import com.madgag.logic.signals.triggers.Criterion.Trigger.MatchingChannels
import com.madgag.logic.time.Time
import com.madgag.logic.time.Time.*
import com.madgag.scala.collection.decorators.*
import spire.math.*
import spire.math.interval.{Closed, Open}

import scala.collection.AbstractIterable
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.ListBuffer
import scala.math.Ordered.orderingToOrdered

/**
 * Represents the [[Signal]]s for multiple channels captured at the same time.
 *
 * [[https://www.tek.com/en/documents/primer/logic-analyzer-fundamentals]]
 */
case class ChannelSignals[T : Time, C](data: Map[C, Signal[T]]) {
  require(data.nonEmpty)
  
  def channelSignalsFor[C1 >: C](subject: ChannelGroup[C1]): Iterable[(C, Signal[T])] =
    data.filter((chan, _) => subject.includes(chan))

  def eventsFor(signalChange: SignalChange[C], direction: Direction): Iterable[Event[T, MatchingChannels[C]]] = {
    val eventIterables: Seq[Iterable[Event[T, C]]] = for {
      (channel, signal) <- channelSignalsFor(signalChange.channelGroup).toSeq
    } yield signal.events(direction)
      .filter(signalChange.triggeredByValueOf) // TODO - can probably do better than repeating filter ops, given that we know that booleans oscillate predictably
      .map(_.map(_ => channel))

    given Ordering[T] = direction.ordering(summon[Time[T]].ev)

    new AbstractIterable[Event[T, MatchingChannels[C]]] {
      override def iterator: Iterator[Event[T, MatchingChannels[C]]] = mergeIterable(eventIterables *).iterator
        .groupAdjacent(e => e.time -> e.value)((t, channels) => Event(t, MatchingChannels(channels.toSet)))
    }
  }

  val interval: BoundedInterval[T] = data.values.map(_.interval).reduce(_ boundedUnion  _)

  lazy val isConstant: Boolean = data.values.forall(_.isConstant)
  
  lazy val changeTimes: SortedSet[T] = data.values.map(_.eventTimes()).reduce(_ ++ _)
  
  lazy val changeAndBoundTimes: SortedSet[T] = changeTimes ++ interval.valueBounds
  
  def at(time: T): Set[C] = data.filter(_._2.state(time)).keySet
  
  def chunksWhile(channel: C, value: Boolean): Iterable[ChannelSignals[T, C]] = for {
    interval <- data(channel).intervalsWhile(value)
  } yield unsafeSubInterval(interval)
  
  def splitOn(channel: C, goingToValue: Boolean): Iterable[ChannelSignals[T, C]] = {
    val goingTimes = data(channel).goingTo(goingToValue).toSeq
    // println(s"goingTimes=$goingTimes")
    for {
      (startTime, endTimeOpt) <- goingTimes.zip(goingTimes.tail.map(Some(_)) :+ None)
    } yield unsafeSubInterval(Interval.fromBounds(Closed(startTime), endTimeOpt.map(Open(_)).getOrElse(interval.upperBound)))
  }

  def unsafeSubInterval(interval: Interval[T]): ChannelSignals[T, C] = transform(_.unsafeSubInterval(interval))
    
  def transform(f: Signal[T] => Signal[T]): ChannelSignals[T, C] = ChannelSignals(data.mapV(f))
  
  def mapKeys[D](f: C => D): ChannelSignals[T, D] = {
    copy(
      data = data.map {
        case (c, s) => (f(c), s)
      }
    )
  }
  
  def merge(other: ChannelSignals[T, C]): ChannelSignals[T, C] = copy(data = data ++ other.data)

  lazy val summary: String = {
    type Pair = (Signal[T], Set[C])
    val booms: Seq[Pair] = data.groupUp(_._2)(_.keySet).toSeq.sortBy(_._2.size)
    def boo(f: Set[C] => String)(sig: Signal[T], chans: Set[C]): String =
      s"${f(chans)}: ${sig.summary}"

    val (allButLast, last) = booms.splitAt(booms.length-1)
    (allButLast.map(boo(_.mkString(","))) ++ last.map(boo(x => if (x.size > 2) "*" else x.mkString(",")))).mkString("\n")
  }
}

object ChannelSignals {
  def from[T: Time, C](timeAndStates: SortedMap[T, Map[C, Boolean]]): ChannelSignals[T, C] = {
    val endTime = timeAndStates.keySet.last

    val eventsByChannel = collection.mutable.Map.empty[C, ListBuffer[Event[T, Boolean]]]

    for {
      (time, states) <- timeAndStates
      (channel, value) <- states
    } eventsByChannel.getOrElseUpdate(channel, new ListBuffer[Event[T, Boolean]]) += Event(time, value)

    ChannelSignals(eventsByChannel.mapV { events =>
      Signal(BoundedInterval.closed(events.head.time, endTime), events)
    })
  }

  extension [A](it: Iterator[A])
    /**
     * Iterating over elements, work out a key and value for each item,
     * then for each group of values with the same key, create a final
     * item value.
     */
    def groupAdjacent[K, B, C](f: A => (K, B))(g: (K, Seq[B]) => C): Iterator[C] = new Iterator[C] {
      private val buffered: scala.collection.BufferedIterator[A] = it.buffered

      override def hasNext: Boolean = buffered.hasNext

      override def next(): C = {
        val (key, initialB) = f(buffered.next())
        val group = Vector.newBuilder[B]
        group += initialB

        var candidate: (K, B) = null
        while (buffered.hasNext && {
          candidate = f(buffered.head)
          candidate._2 == key
        }) {
          group += candidate._2
          buffered.next() // advance, we already got the value using 'head'
        }

        g(key, group.result())
      }
    }
}