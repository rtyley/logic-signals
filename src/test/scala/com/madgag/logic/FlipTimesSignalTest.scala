package com.madgag.logic

import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.time.Time.*
import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.{Inspectors, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import spire.math.Interval
import spire.math.Interval.{atOrAbove, below}

import java.time.Duration
import java.time.Duration.{ZERO, ofSeconds}

class FlipTimesSignalTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks with OptionValues {

  val epsilon: Duration = Duration.ofNanos(1)

  given Arbitrary[FlipTimesSignal[Delta]] = TestKit.arbFlipTimesSignal()

  "Constructor" should "be good for one flip in the middle of a closed interval" in {
    val signal = FlipTimesSignal(
      closed(ofSeconds(10), ofSeconds(15)),
      initialState = true,
      flipTimes = IndexedSeq(ofSeconds(12))
    )

    signal.state(ofSeconds(10)) shouldBe true
    signal.state(ofSeconds(12)) shouldBe false
    signal.state(ofSeconds(15)) shouldBe false
    signal.events(Direction.Asc) shouldBe Seq(Event(ofSeconds(12), false))
    signal.durations() shouldBe Seq(ofSeconds(2) -> true, ofSeconds(3) -> false)
    signal.intervals() shouldBe Seq(
      openUpper(ofSeconds(10), ofSeconds(12)) -> true,
      closed(ofSeconds(12), ofSeconds(15)) -> false
    )
  }

  it should "be good for two flips in the middle of a closed interval" in {
    val signal = FlipTimesSignal(
      closed(ofSeconds(10), ofSeconds(15)),
      initialState = true,
      flipTimes = IndexedSeq(ofSeconds(12), ofSeconds(13))
    )

    signal.state(ofSeconds(10)) shouldBe true
    signal.state(ofSeconds(12)) shouldBe false
    signal.state(ofSeconds(13)) shouldBe true
    signal.state(ofSeconds(15)) shouldBe true
    signal.events(Direction.Asc) shouldBe Seq(Event(ofSeconds(12), false), Event(ofSeconds(13), true))
    signal.events(Direction.Desc) shouldBe Seq(Event(ofSeconds(13), true), Event(ofSeconds(12), false))
    signal.durations() shouldBe Seq(ofSeconds(2) -> true, ofSeconds(1) -> false, ofSeconds(2) -> true)
    signal.intervals() shouldBe Seq(
      openUpper(ofSeconds(10), ofSeconds(12)) -> true,
      openUpper(ofSeconds(12), ofSeconds(13)) -> false,
      closed(ofSeconds(13), ofSeconds(15)) -> true
    )
  }

  it should "be good for one flip in the middle of an open interval" in {
    val signal = FlipTimesSignal(
      open(ofSeconds(10), ofSeconds(15)),
      initialState = true,
      flipTimes = IndexedSeq(ofSeconds(12))
    )

    signal.state(ofSeconds(10).plus(epsilon)) shouldBe true
    signal.state(ofSeconds(12)) shouldBe false
    signal.state(ofSeconds(15).minus(epsilon)) shouldBe false
    signal.events(Direction.Asc) shouldBe Seq(Event(ofSeconds(12), false))
    signal.durations() shouldBe Seq(ofSeconds(2) -> true, ofSeconds(3) -> false)
    signal.intervals() shouldBe Seq(
      open(ofSeconds(10), ofSeconds(12)) -> true,
      openUpper(ofSeconds(12), ofSeconds(15)) -> false
    )
  }

  it should "be allowed a flipTime at the upper bound (unless we can think of a reason to disallow that?)" in {
    val signal = FlipTimesSignal(
      openLower(ofSeconds(10), ofSeconds(15)),
      initialState = true,
      flipTimes = IndexedSeq(ofSeconds(15))
    )

    signal.state(ofSeconds(10).plus(epsilon)) shouldBe true
    signal.state(ofSeconds(15).minus(epsilon)) shouldBe true
    signal.state(ofSeconds(15)) shouldBe false
    signal.events(Direction.Asc) shouldBe Seq(Event(ofSeconds(15), false))
    signal.durations() shouldBe Seq(ofSeconds(5) -> true, ZERO -> false)
    signal.intervals() shouldBe Seq(
      open(ofSeconds(10), ofSeconds(15)) -> true,
      point(ofSeconds(15)) -> false
    )
  }

  it should "HEM-hem: not lose events as we take sub-intervals" in {
    val interval = closed(ofSeconds(10), ofSeconds(15))
    val signal = FlipTimesSignal(
      interval,
      initialState = true,
      flipTimes = IndexedSeq(ofSeconds(12), ofSeconds(13))
    )

    val splitPoint = ofSeconds(12)
    val subSignals =
      Seq(interval.intersect(below(splitPoint)), interval.intersect(atOrAbove(splitPoint))).map(signal.unsafeSubInterval)

    subSignals.map(_.events(Direction.Asc)).reduce(_ ++ _) shouldBe signal.events(Direction.Asc)
    subSignals.map(_.intervals()).reduce(_ ++ _) shouldBe signal.intervals()
  }


  it should "work for lots" in forAll { (signal: FlipTimesSignal[Delta]) =>
    println(signal.summary)
    val intervals = signal.intervals()
    Inspectors.forAll(intervals) {
      case (interval, state) =>
        interval ⊆ signal.interval shouldBe true
    }

    intervals.map(_._1).foldLeft(List[Interval[Delta]](signal.interval)) {
      case (remainingSignalIntervals, subInterval) => remainingSignalIntervals.flatMap(_ -- subInterval)
    } shouldBe empty

    intervals.map(_._1.duration).reduce(_ plus _) shouldBe signal.interval.duration
    Inspectors.forAll(intervals.zip(intervals.tail)) {
      case ((interval, state), (nextInterval, nextState)) =>
        nextState shouldBe !state
        interval.overlap(nextInterval).isDisjoint shouldBe true
        val commonBound = nextInterval.lowerValueBound.a
        interval.upperValueBound.a shouldBe commonBound
        interval.contains(commonBound) || nextInterval.contains(commonBound) shouldBe true
    }

    val eventsInAscendingDirection = signal.events(Direction.Asc)
    eventsInAscendingDirection.size shouldBe signal.flipTimes.size
    Inspectors.forAll(eventsInAscendingDirection) { event =>
      event.value shouldBe signal.state(event.time)
    }
    signal.events(Direction.Desc).reverse shouldEqual eventsInAscendingDirection

    Inspectors.forAll(signal.goingTo(true)) { flipTime =>
      signal.state(flipTime) shouldBe true
      intervals.find(_._1.lowerValueBound.a == flipTime).value._2 shouldBe true
    }
  }
}
