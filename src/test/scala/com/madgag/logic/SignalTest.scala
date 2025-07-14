package com.madgag.logic

import com.madgag.logic.TestKit.signalFor
import com.madgag.logic.Time.Delta
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spire.math.Interval
import spire.math.Interval.fromBounds
import spire.math.interval.{Closed, Open, Unbound}

import java.time.Duration
import java.time.Duration.ofMillis
import Time.*

class SignalTest extends AnyFlatSpec with should.Matchers {

  def eventsFor(timeline: String): Seq[Event[Delta, Boolean]] = for {
    (x, timeMs) <- timeline.zipWithIndex if x != '.'
  } yield Event(ofMillis(timeMs), x == '1')

  def convert(timeline: String): Signal[Delta] = Signal(eventsFor(timeline))

  def convert(signal: Signal[Delta], len: Int): String = {
    val events = signal.events()
    val eventMap = events.map(e => e.time.toMillis -> e.value).toMap
    (for {
      timeMs <- 0 until len
    } yield eventMap.get(timeMs).map(v => if (v) '1' else '0').getOrElse('.')).mkString
  }

  def checkSignalEvents(input: String): Unit = {
    val events = eventsFor(input)
    val signal = Signal(events)
    signal.events() shouldBe events
  }

  def checkDeglitch(input: String, expectedOutput: String): Unit = {
    val deglitchedSignal = convert(input).deglitch(ofMillis(2))
    convert(deglitchedSignal, input.length) shouldBe expectedOutput
  }

  "Signal events" should "be correct" in {
    checkSignalEvents("0")
    checkSignalEvents("1")
    checkSignalEvents("01")
    checkSignalEvents("10")
    checkSignalEvents("101")
    checkSignalEvents("010")
    checkSignalEvents(".010.")
    checkSignalEvents("0...1..0")
  }

  "Deglitch" should "handle the small stuff" in checkDeglitch(
    ".101..0",
    "...1..0"
  )

  it should "foo" in checkDeglitch(
    "..101..1..0",
    "....1.....0"
  )

  "Signal.state" should "be correct" in {
    val signal = signalFor("█▁▁▁▁▁▁▁▁▁▁▁")
    signal.state(ofMillis(0)) shouldBe true
    signal.state(ofMillis(1)) shouldBe false
    signal.state(ofMillis(2)) shouldBe false
  }

  "Signal.intervalsWhile" should "be correct" in {
    signalFor("█▁▁").intervalsWhile(false) shouldBe Seq(
      fromBounds(Closed(ofMillis(1)), Closed(ofMillis(2)))
    )

    signalFor("█▁▁").intervalsWhile(true) shouldBe Seq(fromBounds(Closed(ofMillis(0)), Open(ofMillis(1))))

  }

  it should "split a signal interval into parts that completely cover the interval without overlapping" in {
    val originalSignal = signalFor("█▁▁█▁██")
    println(originalSignal.interval)

    val lowIntervals = originalSignal.intervalsWhile(false)
    val highIntervals = originalSignal.intervalsWhile(true)
    val allIntervals = (lowIntervals ++ highIntervals).toSeq
    allIntervals.reduce(_ | _) shouldBe originalSignal.interval
    for {
      lowInterval <- lowIntervals
      highInterval <- highIntervals
    } {
      lowInterval.intersects(highInterval) shouldBe false
    }
  }

}
