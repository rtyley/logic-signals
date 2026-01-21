package com.madgag.logic.protocol.holtek.ht1632c.signals

import com.madgag.logic.TestKit.{CharDuration, signals}
import com.madgag.logic.protocol.holtek.ht1632c.Channel.ChipSelect.Leader
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Clock.{Read, Write}
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Data
import com.madgag.logic.protocol.holtek.ht1632c.signals.TimingCharacteristics.{dataSetupAndHoldAroundClock, tod, violationFinder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration.ofNanos

class TimingCharacteristicsTest extends AnyFlatSpec with should.Matchers {
  given CharDuration = CharDuration(ofNanos(50))

  "Characteristics validation" should "be fine with a Data change that comes promptly in a negative Read-Clock pulse" in {
    tod.violationsIn(signals(
      Read -> "█▁▁▁▁▁▁██",
      Data -> "▁▁▁██████"
    )) shouldBe empty
  }

  it should "spot a Data change that comes too late in a negative Read-Clock pulse" in {
    tod.violationsIn(signals(
      Read -> "█▁▁▁▁▁▁██",
      Data -> "▁▁▁▁▁▁███"
    )) should have size 1
  }

  it should "disregard a Data change that comes after a negative Read-Clock pulse has ended" in {
    tod.violationsIn(signals(
      Read -> "█▁▁▁▁▁▁██",
      Data -> "▁▁▁▁▁▁▁▁█"
    )) shouldBe empty
  }

  it should "be fine with a Clock rise that leaves sufficient time before and after Data changes" in {
    dataSetupAndHoldAroundClock.violationsIn(signals(
      Data -> "█▁▁▁▁▁▁█",
      Read -> "▁▁▁█████"
    )) shouldBe empty
  }

  it should "spot a Clock rise that occurs too soon after a Data change" in {
    dataSetupAndHoldAroundClock.violationsIn(signals(
      Data -> "█▁▁▁▁▁▁█",
      Read -> "▁███████"
    )).keySet.map(_.name) shouldBe Set("tsu")
  }

  it should "spot a Clock rise that doesn't have enough time before the next Data change" in {
    dataSetupAndHoldAroundClock.violationsIn(signals(
      Data -> "█▁▁▁▁▁▁█",
      Read -> "▁▁▁▁▁▁██"
    )).keySet.map(_.name) shouldBe Set("th")
  }

  it should "not require the time from Leader fall to Write fall to be as long as the required clock pulse length" in {
    given CharDuration = CharDuration(ofNanos(100))

    violationFinder.violationsIn(signals(
      Leader -> "███▁▁▁▁▁▁▁▁▁▁███",
      Write  -> "██████▁▁▁▁▁█████"
    )) shouldBe empty
  }
}
