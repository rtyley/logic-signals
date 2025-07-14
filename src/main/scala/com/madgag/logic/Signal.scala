package com.madgag.logic

import cats.kernel.Order
import cats.kernel.Order.*
import com.madgag.logic.*
import com.madgag.logic.Time.*
import spire.math.Interval
import spire.math.interval.{Closed, Open, Unbound, ValueBound}

import java.time.Duration
import scala.collection.Searching.*
import scala.collection.immutable.SortedSet
import scala.math.Ordering.Implicits.*

trait Signal[T: Time] {
  val interval: Interval[T]
  
  val isConstant: Boolean
  
  def state(time: T): Boolean

  def goingTo(state: Boolean): Iterable[T]

  def intervalsWhile(state: Boolean): Iterable[Interval[T]]

  def subInterval(interval: Interval[T]): Signal[T]

  def deglitch(threshold: Duration): Signal[T]

  def events(): Seq[Event[T, Boolean]]
  
  def eventTimes(): SortedSet[T]
}

object Signal {
  private case class PunkSignal[T: Time](interval: Interval[T], initialState: Boolean, flipTimes: IndexedSeq[T]) extends Signal[T] {
    assert(flipTimes.isEmpty || flipTimes.zip(flipTimes.tail).forall((a,b) => a < b))

    override val isConstant: Boolean = flipTimes.isEmpty
    
    def stateFor(flipIndex: Int) = ((flipIndex % 2) == 0) ^ initialState

    override def state(time: T): Boolean = stateFor(flipTimes.search(time) match {
      case InsertionPoint(insertionPoint) => insertionPoint - 1
      case Found(index) => index
    })

    override def goingTo(state: Boolean): Iterable[T] =
      flipTimes.indices.filter(stateFor(_) == state).map(flipTimes)

    override def intervalsWhile(state: Boolean): Iterable[Interval[T]] = {
      val prefixedTimes = (if (initialState == state) Seq(None) else Seq.empty) ++ flipTimes.map(Some(_))
      val suffixedTimes = prefixedTimes ++ (if (prefixedTimes.size % 2 == 1) Seq(None) else Seq.empty)

      suffixedTimes.grouped(2).map { events =>
        Interval.fromBounds(events(0).map(Closed(_)).getOrElse(Unbound()), events(1).map(Open(_)).getOrElse(
          Unbound()
        )).intersect(interval)
      }.filter(!_.isEmpty).toSeq
    }

    override def subInterval(sub: Interval[T]): Signal[T] =
      val effectiveInterval = sub.intersect(sub)
      effectiveInterval.lowerBound match {
        case ValueBound(a) =>
          PunkSignal(effectiveInterval, state(a), flipTimes.subInterval(Interval.fromBounds(Open(a), sub.upperBound)))
        case _ => PunkSignal(effectiveInterval, initialState, flipTimes.subInterval(sub))
      }

    override def deglitch(threshold: Duration): PunkSignal[T] = PunkSignal(
      interval, initialState, vorg(flipTimes) {
        case (acc, lastEvent, event) =>
          val glitch = Time.between(lastEvent, event) < threshold
          if (glitch) acc.dropRight(1) else acc :+ event
      }
    )

    override def events(): Seq[Event[T, Boolean]] = for {
      (time, index) <- flipTimes.zipWithIndex
    } yield Event(time, stateFor(index))
    
    override def eventTimes(): SortedSet[T] = SortedSet.from(flipTimes)
  }

  private def vorg[X](items: Iterable[X])(boom: (Vector[X], X, X) => Vector[X]): IndexedSeq[X] = items.foldLeft(Vector.empty[X]) {
    case (acc, item) => acc.lastOption match {
      case Some(previousItem) => boom(acc, previousItem, item)
      case None => Vector(item)
    }
  }

  def apply[T: Time](events: Iterable[Event[T, Boolean]]): Signal[T] = {
    val flipEvents = vorg(events) {
      case (acc, lastEvent, event) => if (event.value == lastEvent.value) acc else acc :+ event
    }
    PunkSignal(
      Interval(events.head.time, events.last.time),
      !events.head.value,
      flipEvents.map(_.time)
    )
  }
}

