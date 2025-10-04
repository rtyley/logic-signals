package com.madgag.logic

import com.madgag.logic.Time.Delta

import java.time.Duration.ofMillis
import com.madgag.scala.collection.decorators.*
import com.madgag.logic.Time.*
import org.scalatest.matchers.should.Matchers.shouldBe
import spire.math.Interval

object TestKit {
  val High: Set[Char] = Set('1', '█')

  def eventsFor(timeline: String): Seq[Event[Delta, Boolean]] = for {
    (x, timeMs) <- timeline.zipWithIndex if x != '.'
  } yield Event(ofMillis(timeMs), High(x))

  def signalFor(timeline: String): Signal[Delta] = {
    val sig = Signal(Interval.openUpper(ofMillis(0), ofMillis(timeline.length)), eventsFor(timeline))
    sig.interval.duration shouldBe ofMillis(timeline.length)
    sig
  }

  def signals[T](sigs: (T, String)*) = ChannelSignals[Delta, T](sigs.toMap.mapV(signalFor))

}
