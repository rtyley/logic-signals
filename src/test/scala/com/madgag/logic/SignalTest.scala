package com.madgag.logic

import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.TestKit.{CharDuration, displayCharFor, signalFor}
import com.madgag.logic.time.Time.*
import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.{Inspectors, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import spire.math.Interval
import spire.math.Interval.{fromBounds, openUpper}
import spire.math.interval.{Closed, Open}

import java.time.Duration
import java.time.Duration.{ofMillis, ofSeconds}

class SignalTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks with OptionValues {
  given CharDuration = CharDuration(ofMillis(1))
  
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 300)

  given Arbitrary[Signal[Delta]] = TestKit.arbSignal()

  def convert(timeline: String): Signal[Delta] = {
    val signal = signalFor(timeline)
    require(convert(signal, timeline.length) == timeline, s"Could not round-trip '$timeline'")
    signal
  }

  def convert(signal: Signal[Delta], len: Int): String = (for {
    index <- 0 until len
  } yield displayCharFor(signal.state(ofMillis(index)))).mkString

  def checkDeglitch(input: String, expectedOutput: String): Unit = {
    val deglitchedSignal = convert(input).deglitch(ofMillis(2))
    convert(deglitchedSignal, input.length) shouldBe expectedOutput
  }


  "Deglitch" should "handle the small stuff" in checkDeglitch(
    "▁█▁███▁",
    "▁▁▁███▁"
  )

  "Signal.forIntervalImpliedBy" should "recognise that the first and last rows are not events" in {
    val signal = Signal.forIntervalImpliedBy(Seq(
      Event(ofSeconds(10), false),
      Event(ofSeconds(12), true),
      Event(ofSeconds(17), false),
      Event(ofSeconds(20), false) // last point isn't a change, it just reiterates the state at the end of the interval
    ))
    signal.interval shouldBe Interval.closed(ofSeconds(10), ofSeconds(20))
    signal.events(Direction.Asc) shouldBe Seq(Event(ofSeconds(12), true), Event(ofSeconds(17), false))
    signal.intervals() shouldBe Seq(
      BoundedInterval.openUpper(ofSeconds(10), ofSeconds(12)) -> false,
      BoundedInterval.openUpper(ofSeconds(12), ofSeconds(17)) -> true,
      BoundedInterval.closed(ofSeconds(17), ofSeconds(20)) -> false
    )
  }

  "Signal.state" should "be correct" in {
    val signal = signalFor("█▁▁")
    signal.interval.duration shouldBe ofMillis(3)
    signal.state(ofMillis(0)) shouldBe true
    signal.state(ofMillis(1)) shouldBe false
    signal.state(ofMillis(2)) shouldBe false
  }

  "Signal.intervals" should "just contain the 'interval' if there are no flipTimes" in {
    val lowSignal = signalFor("▁")
    lowSignal.interval.duration shouldBe ofMillis(1)
    lowSignal.intervals().toSeq shouldBe Seq(lowSignal.interval -> false)

    val highSignal = signalFor("██")
    highSignal.interval.duration shouldBe ofMillis(2)
    highSignal.intervals().toSeq shouldBe Seq(highSignal.interval -> true)
  }

  it should "have bounds which are either the bounds of 'interval', or flipTimes" in {

  }

  it should "work for a specific case" in {
    val bi = BoundedInterval.openLower(ofSeconds(0), ofSeconds(3))
    val samples = Seq(ofSeconds(1) -> true).map(Event(_, _))

    val signal = Signal(bi, samples)
    signal.state(ofSeconds(1)) shouldBe true
  }

  it should "work for lots"  in forAll { (signal: Signal[Delta]) =>
    println(signal.summary)
    val intervals = signal.intervals()
    intervals.map(_._1.duration).reduce(_ plus _) shouldBe signal.interval.duration
    Inspectors.forAll(intervals.zip(intervals.tail)) {
      case ((interval, state) , (nextInterval, nextState)) =>
        nextState shouldBe !state
        interval.overlap(nextInterval).isDisjoint shouldBe true
    }

    Inspectors.forAll(signal.goingTo(true)) { flipTime =>
      signal.state(flipTime) shouldBe true
      intervals.find(_._1.lowerValueBound.a == flipTime).value._2 shouldBe true
    }
  }

  "Signal.intervalsWhile" should "be correct" in {
    val signal = signalFor("█▁▁")

    signal.intervalsWhile(false) shouldBe Seq(openUpper(ofMillis(1), ofMillis(3)))
    signal.intervalsWhile(true) shouldBe Seq(fromBounds(Closed(ofMillis(0)), Open(ofMillis(1))))
  }

  "Signal.durations" should "be correct" in {
    val sig = signalFor("█▁▁")
    sig.interval.duration shouldBe ofMillis(3)
    sig.durations() shouldBe Seq(
      ofMillis(1) -> true,
      ofMillis(2) -> false
    )
    sig.durations().map(_._1).reduce(_ plus _) shouldBe ofMillis(3)
  }

  "Signal.summary" should "be correct" in {
    val sig = signalFor("█▁▁")
    sig.summary shouldBe "↗ 1ms ↘ 2ms"
  }
  
  it should "split a signal interval into parts that completely cover the interval without overlapping" in {
    val originalSignal = signalFor("█▁▁█▁██")
    println(originalSignal.interval)

    val lowIntervals = originalSignal.intervalsWhile(false)
    val highIntervals = originalSignal.intervalsWhile(true)
    val allIntervals = (lowIntervals ++ highIntervals).toSeq
    allIntervals.reduce(_ boundedUnion _) shouldBe originalSignal.interval
    for {
      lowInterval <- lowIntervals
      highInterval <- highIntervals
    } {
      lowInterval.intersects(highInterval) shouldBe false
    }
  }

  it should "be able to extract subintervals" in {
    val originalSignal = signalFor("█▁▁█▁██")
    println(originalSignal.interval)

    val interval = openUpper(ofMillis(3), ofMillis(6))
    val narrowSignal = signalFor("▁▁▁█▁█▁")
    narrowSignal.unsafeSubInterval(interval) shouldBe originalSignal.unsafeSubInterval(interval)
  }

  it should "recognise that we can extend the interval when we do subintervals" in {
    val interval = openUpper(ofMillis(-3), ofMillis(2))
    signalFor("█▁▁▁▁▁▁").unsafeSubInterval(interval) shouldBe signalFor("█▁")
  }
}
