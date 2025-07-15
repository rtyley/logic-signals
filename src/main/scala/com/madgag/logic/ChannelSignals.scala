package com.madgag.logic

import com.madgag.logic.Time.*
import com.madgag.scala.collection.decorators.*
import spire.*
import spire.math.*
import spire.math.interval.{Closed, Open}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.ListBuffer

case class ChannelSignals[T : Time, C](data: Map[C, Signal[T]]) {

  val interval: Interval[T] = data.values.map(_.interval).fold(Interval.empty[T])(_ | _)

  lazy val isConstant: Boolean = data.values.forall(_.isConstant)
  
  lazy val changeTimes: SortedSet[T] = data.values.map(_.eventTimes()).reduce(_ ++ _)
  
  lazy val changeAndBoundTimes: SortedSet[T] = changeTimes ++ interval.valueBounds
  
  def at(time: T): Set[C] = data.filter(_._2.state(time)).keySet
  
  def chunksWhile(channel: C, value: Boolean): Iterable[ChannelSignals[T, C]] = for {
    interval <- data(channel).intervalsWhile(value)
  } yield subInterval(interval)
  
  def splitOn(channel: C, goingToValue: Boolean): Iterable[ChannelSignals[T, C]] = {
    val goingTimes = data(channel).goingTo(goingToValue).toSeq
    // println(s"goingTimes=$goingTimes")
    for {
      (startTime, endTimeOpt) <- goingTimes.zip(goingTimes.tail.map(Some(_)) :+ None)
    } yield subInterval(Interval.fromBounds(Closed(startTime), endTimeOpt.map(Open(_)).getOrElse(interval.upperBound)))
  }

  def subInterval(interval: Interval[T]): ChannelSignals[T, C] = transform(_.subInterval(interval))
    
  def transform(f: Signal[T] => Signal[T]): ChannelSignals[T, C] = ChannelSignals(data.mapV(f))
  
  def mapKeys[D](f: C => D): ChannelSignals[T, D] = {
    copy(
      data = data.map {
        case (c, s) => (f(c), s)
      }
    )
  }
  
  def merge(other: ChannelSignals[T, C]): ChannelSignals[T, C] = copy(data = data ++ other.data)
}

object ChannelSignals {
  def from[T: Time, C](timeAndStates: SortedMap[T, Map[C, Boolean]]): ChannelSignals[T, C] = {
    val eventsByChannel = collection.mutable.Map.empty[C, ListBuffer[Event[T, Boolean]]]

    for {
      (time, states) <- timeAndStates
      (channel, value) <- states
    } eventsByChannel.getOrElseUpdate(channel, new ListBuffer[Event[T, Boolean]]) += Event(time, value)

    ChannelSignals(eventsByChannel.mapV(Signal(_)))
  }
}