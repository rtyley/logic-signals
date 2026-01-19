package com.madgag.logic

import com.madgag.logic.TestKit.samplesFor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration.ofMillis

class TestKitTest extends AnyFlatSpec with should.Matchers {
  "TestKitTest.eventsFor" should "give a repeated value for the end of the interval" in {
    samplesFor("▁") shouldBe Seq(
      Event(ofMillis(0), false),
      Event(ofMillis(1), false)
    )

    samplesFor("██") shouldBe Seq(
      Event(ofMillis(0), true),
      Event(ofMillis(1), true),
      Event(ofMillis(2), true)
    )

    samplesFor("▁█") shouldBe Seq(
      Event(ofMillis(0), false),
      Event(ofMillis(1), true),
      Event(ofMillis(2), true)
    )
  }
}