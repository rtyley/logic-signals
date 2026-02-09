package com.madgag.logic

import cats.kernel.Order
import cats.kernel.Order.*
import com.madgag.logic.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.FlipTimesSignal.zipWithFlippingBoolean
import com.madgag.logic.time.Time
import com.madgag.logic.time.Time.*
import spire.math.interval.{Closed, Open, Unbound, ValueBound}
import spire.math.{Empty, Interval}

import java.time.Duration
import scala.collection.Searching.{Found, InsertionPoint}
import scala.collection.immutable.SortedSet
import scala.math.Ordering.Implicits.*

/**
 *
 * @param initialState The state _before_ the first flipTime - which is not necessarily the same state of the first
 *                     _interval_, ie if the first flipTime is at the (closed) lower bound.
 */
protected case class FlipTimesSignal[T: Time](
  interval: BoundedInterval[T],
  initialState: Boolean,
  flipTimes: IndexedSeq[T]
) extends Signal[T] {

  val firstFlipCoincidentWithLowerBound: Boolean = flipTimes.headOption.contains(interval.lowerValueBound.a)
  val stateOfFirstInterval: Boolean = initialState ^ firstFlipCoincidentWithLowerBound

  if (flipTimes.nonEmpty) {
    require(flipTimes.zip(flipTimes.tail).forall((a, b) => a < b))
    require(interval.hasAtOrBelow(flipTimes.head), "First flip time is below the interval")
    require(interval.hasAtOrAbove(flipTimes.last), "Last flip time is above the interval")
  }
  
  override val isConstant: Boolean = flipTimes.isEmpty

  def stateFor(flipIndex: Int) = ((flipIndex % 2) == 0) ^ initialState

  // val finalState = stateFor(flipTimes.size - 1)
  
  override def state(time: T): Boolean = {
    require(interval.contains(time))
    stateFor(flipTimes.search(time) match {
      case InsertionPoint(insertionPoint) => insertionPoint - 1
      case Found(index) => index
    })
  }

  override def goingTo(state: Boolean): Iterable[T] =
    flipTimes.indices.filter(stateFor(_) == state).map(flipTimes)

  /** Unifying these intervals should equal `interval`
   * Also interval bounds must be either the bounds of interval, or flipTimes.
   * Where a flipTime occurs, the interval with that state is closed on its lower bound (the flipTime), while the
   * prior interval is open on that upper bound.
   *
   * A corner case is having the flip time at a bound of the interval (can only happen when the bound is closed).
   *
   * Convention: If the flip time is at the lower (closed) bound of the interval, then 'initialState' is actually the opposite
   * of what the initial interval will denote.
   *
   * Should this thing be able to return intervals that are Point???!!!
   */
  override def intervals(): Iterable[(BoundedInterval[T], Boolean)] = {
    val flipTimesExcludingLowerBound = if (firstFlipCoincidentWithLowerBound) flipTimes.tail else flipTimes
    val innerBounds: Seq[ValueBound[T]] = flipTimesExcludingLowerBound.flatMap(t => Seq(Open(t), Closed(t)))

    LazyList.from(
      (interval.lowerValueBound +: innerBounds :+ interval.upperValueBound).grouped(2).flatMap {
        intervalBounds => Interval.fromBounds(intervalBounds.head, intervalBounds.last).toBoundedIntervalOpt
      }
    ).zipWithFlippingBoolean(stateOfFirstInterval)
  }

  // the total durations should total to the interval duration, or what are we doing?
  // the duration should be paired with the state during that duration
  override def durations(): Iterable[(Duration, Boolean)] = {
    val todoIntervals = intervals().toList
    todoIntervals.map {
      (interval, state) => interval.duration -> state
    }
  }

  override def intervalsWhile(state: Boolean): Iterable[BoundedInterval[T]] = intervals().collect {
    case (interval, s) if s == state => interval
  }

  protected[logic] def unsafeSubInterval(sub: Interval[T]): Signal[T] = interval.boundedIntersect(sub) match {
    case _: Empty[T] => throw new IllegalStateException()
    case effectiveInterval: BoundedInterval[T] =>
      val everythingUpToButExcludingTheLowerBound =
        (~Interval.fromBounds(effectiveInterval.lowerBound, Unbound())).head

      val lowerBound = effectiveInterval.lowerValueBound.a
      val subIntervalFlipTimes = flipTimes.subInterval(effectiveInterval)
      val flip = FlipTimesSignal(
        effectiveInterval,
        state(lowerBound) ^ subIntervalFlipTimes.headOption.contains(lowerBound),
        subIntervalFlipTimes
      )
      flip
  }

  override def deglitch(threshold: Duration): FlipTimesSignal[T] = FlipTimesSignal(
    interval, initialState, Signal.accumulateWithPreviousOver(flipTimes) {
      case (acc, lastEvent, event) =>
        val glitch = Time.between(lastEvent, event) < threshold
        if (glitch) acc.dropRight(1) else acc :+ event
    }
  )

  override def events(direction: Direction): LazyList[Event[T, Boolean]] = for {
    (time, state) <- LazyList.from(direction.approach(flipTimes))
      .zipWithFlippingBoolean(direction.when(stateFor(0), stateFor(flipTimes.size-1)))
  } yield Event(time, state)

  override def eventTimes(): SortedSet[T] = {
    given Direction = Direction.Asc
    SortedSet.from(flipTimes)
  }
}

object FlipTimesSignal {
  extension [A](iter: LazyList[A])
    def zipWithFlippingBoolean(start: Boolean): LazyList[(A, Boolean)] =
      iter.zip(Iterator.iterate(start)(!_))
}