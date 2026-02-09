package com.madgag.logic.protocol.holtek.ht1632c.operations

import com.madgag.logic.bits.Nibble
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.*
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.OffOn.On
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.Switchable.LedDutyCycleGenerator
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.SyncRole.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scodec.bits.bin

class CommandTest extends AnyFlatSpec with should.Matchers {
  "Command" should "parse those nibbles" in {
    // NibblePattern("0000").matches(Nibble(bin"0000")) shouldBe true

    LedDutyCycleGenerator.content.fixedBits.bitVector shouldBe bin"0010"
    LedDutyCycleGenerator.content.mask.bitVector shouldBe bin"1110"

    LedDutyCycleGenerator.content.matches(Nibble(bin"0011")) shouldBe true

    Command(Nibble(bin"0000"), Nibble(bin"0011")) shouldBe LedDutyCycleGenerator(On)

    Command(bin"000100000") shouldBe Follower
    Command(bin"000110000") shouldBe RCLeader
    Command(bin"000111000") shouldBe ExternalClockLeader
    

    Command(bin"101000010") shouldBe PWM(2)
    Command(bin"101000110") shouldBe PWM(4)
    Command(bin"101001110") shouldBe PWM(8)
  }
}