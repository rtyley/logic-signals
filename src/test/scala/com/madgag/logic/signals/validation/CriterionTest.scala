package com.madgag.logic.signals.validation

import com.madgag.logic.TestKit.CharDuration
import com.madgag.logic.signals.triggers.ChannelGroup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration.ofMillis

trait Chans extends ChannelGroup[Chans] {
  override def includes[C1 >: Chans](channel: C1): Boolean = channel == this
}

case object A extends Chans
case object B extends Chans

class CriterionTest extends AnyFlatSpec with should.Matchers {
  given CharDuration = CharDuration(ofMillis(1))

//  "SV" should "validate 'falling' to 'falling'" in {
//    val channelSignals = signals(
//      A -> "█▁▁▁▁▁█▁▁▁▁█▁▁▁█▁▁▁▁▁▁▁",
//      B -> "████▁████▁███▁▁████▁█▁█"
//    )
//    val A_then_B = A.falling -> B.falling
//
//    (A_then_B takes (min = 1.millis)).validate(channelSignals) shouldBe empty
//    (A_then_B takes (min = 2.millis)).validate(channelSignals) should have size 1
//    (A_then_B takes (min = 3.millis)).validate(channelSignals) should have size 2
//  }
//
//  it should "validate 'changing' to 'rising'" in {
//    val channelSignals = ChannelSignals(Map(
//      A -> signalFor("█▁▁▁███▁▁▁▁▁▁"),
//      B -> signalFor("▁▁█▁▁█▁▁█▁▁█▁")
//    ))
//    val A_then_B = A.change -> B.rising
//    (A_then_B takes (min = 1.millis)).validate(channelSignals) shouldBe empty
//    (A_then_B takes (min = 2.millis)).validate(channelSignals) should have size 3
//  }
}
