package com.madgag.logic.protocol.holtek.ht1632c.signals

import com.madgag.logic.TestKit.{CharDuration, signals}
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Clock.Read
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Data
import com.madgag.logic.protocol.holtek.ht1632c.signals.TimingCharacteristics.{dataSetupAndHoldAroundClock, tod}
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
}
