package com.madgag.logic

import cats.kernel.Order
import cats.kernel.Order.*
import com.gu.time.duration.formatting.*
import com.madgag.logic.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.time.Time
import com.madgag.logic.time.Time.*
import spire.math.Interval

import java.time.Duration
import java.time.temporal.ChronoUnit.NANOS
import scala.collection.immutable.SortedSet
import scala.math.Ordering.Implicits.*

/**
 * A binary (or 'logic') signal [[https://en.wikipedia.org/wiki/Digital_signal]].
 *
 * @tparam T allows times in the signal to be represented by an absolute value (like [[java.time.Instant]])
 *           or a value relative to the start of the capture, eg [[Delta]] ([[Duration]])
 */
trait Signal[T: Time] {
  val interval: BoundedInterval[T]
  
  val isConstant: Boolean
  
  def state(time: T): Boolean

  def goingTo(state: Boolean): Iterable[T]

  def intervals(): Iterable[(BoundedInterval[T], Boolean)]

  def intervalsWhile(state: Boolean): Iterable[BoundedInterval[T]]

  def durations(): Iterable[(Duration, Boolean)]

  /**
   * This method can throw an exception, if the supplied interval doesn't overlap with the existing signal interval.
   */
  protected[logic] def unsafeSubInterval(interval: Interval[T]): Signal[T]

  def deglitch(threshold: Duration): Signal[T]

  def events(direction: Direction): Seq[Event[T, Boolean]]
  
  def eventTimes(): SortedSet[T]


  lazy val summary: String = durations().map {
    (duration: Duration, g: Boolean) => s"${if (g) "↗" else "↘"} ${duration.format(2, NANOS)}"
  }.mkString(" ")
}

object Signal {

  enum ChangeType(val char: Char, val goingTo: Set[Boolean]):
    case Rising extends ChangeType('↑', Set(true))
    case Falling extends ChangeType('↓', Set(false))
    case Change extends ChangeType('↕', Set(false, true))

  def accumulateWithPreviousOver[X](items: Iterable[X])(boom: (Vector[X], X, X) => Vector[X]): IndexedSeq[X] = items.foldLeft(Vector.empty[X]) {
    case (acc, item) => acc.lastOption match {
      case Some(previousItem) => boom(acc, previousItem, item)
      case None => Vector(item)
    }
  }

  def apply[T: Time](intervals: Seq[(BoundedInterval[T], Boolean)]): Signal[T] = {
    require(intervals.nonEmpty, "We require at least 1 interval")
    require(intervals.zip(intervals.tail).forall {
      case ((interval, state), (nextInterval, nextState)) =>
        nextState != state
          && interval.upperValueBound.a == nextInterval.lowerValueBound.a
          && interval.overlap(nextInterval).isDisjoint
    })

    val signal = FlipTimesSignal(
      intervals.head._1.boundedUnion(intervals.last._1),
      intervals.head._2, // what is initial state?!
      intervals.tail.map(_._1.lowerValueBound.a).toIndexedSeq
    )
    require(signal.intervals() == intervals)
    signal
  }

  /**
   *
   * @param interval may be larger than the time-span covered by the samples?
   * @param samples non-empty, for distinct times within the interval, may repeat state.
   *                The first sample will *not* be interpreted as a flip-event, but as the initial value of
   *                the signal at the start of the interval.
   */
  def apply[T: Time](interval: BoundedInterval[T], samples: Iterable[Event[T, Boolean]]): Signal[T] = {
    require(samples.nonEmpty, "Without 1 sample, we can't determine the state at any point in the interval")
    require(samples.zip(samples.tail).forall((a, b) => a.time < b.time))
    require(samples.forall(e => interval.contains(e.time)))

    val eventsWithDeduplicatedStates = accumulateWithPreviousOver(samples) {
      case (acc, lastEvent, event) => if (event.value == lastEvent.value) acc else acc :+ event
    }

    FlipTimesSignal(
      interval,
      samples.head.value,
      flipTimes = eventsWithDeduplicatedStates.tail.map(_.time)
    )
  }

  /**
   * @param samples signals where state may possibly be repeated, e.g. signal samples
   *                at 1ns resolution where the signal only changes with 100ns
   */
  def forIntervalImpliedBy[T: Time](samples: Iterable[Event[T, Boolean]]): Signal[T] = {

    Signal(BoundedInterval.closed(samples.head.time, samples.last.time), samples)
  }
}

