package com.madgag.logic.protocol.holtek.ht1632c

import com.madgag.logic.TestKit.signalFor
import com.madgag.logic.protocol.holtek.ht1632c.SignalValidation.validate
import com.madgag.logic.{ChannelSignals, Time}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.duration.DurationInt

trait Chans extends CriteriaSubject[Chans] {
  override val appliesTo: Chans => Boolean = _ == this
}

case object A extends Chans
case object B extends Chans

class SignalValidationTest extends AnyFlatSpec with should.Matchers {
  "SV" should "validate 'falling' to 'falling'" in {
    val channelSignals = ChannelSignals(Map(
      A -> signalFor("█▁▁▁▁▁█▁▁▁▁█▁▁▁█▁▁▁▁▁▁▁"),
      B -> signalFor("████▁████▁███▁▁████▁█▁█")
    ))
    val A_then_B = A.falling -> B.falling
    validate(channelSignals, A_then_B takes (min = 1.millis)) shouldBe empty
    validate(channelSignals, A_then_B takes (min = 2.millis)) should have size 1
    validate(channelSignals, A_then_B takes (min = 3.millis)) should have size 2
  }

  it should "validate 'changing' to 'rising'" in {
    val channelSignals = ChannelSignals(Map(
      A -> signalFor("█▁▁▁███▁▁▁▁▁▁"),
      B -> signalFor("▁▁█▁▁█▁▁█▁▁█▁")
    ))
    val A_then_B = A.change -> B.rising
    validate(channelSignals, A_then_B takes (min = 1.millis)) shouldBe empty
    validate(channelSignals, A_then_B takes (min = 2.millis)) should have size 3
  }
}
