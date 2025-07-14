package com.madgag.logic

import com.madgag.logic.BitEndian.{BigFirst, LittleFirst}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scodec.bits.bin

class BitEndianTest extends AnyFlatSpec with should.Matchers {
  "BitEndian" should "make sense" in {
    val bv = BigFirst.convert(Seq(true, false, false))
    bv shouldBe bin"100"
    bv.toInt(signed = false) shouldBe 4
  }

  it should "be cool with LittleFirst" in {
    val bv = LittleFirst.convert(Seq(false, false, true))
    bv shouldBe bin"100"
    bv.toInt(signed = false) shouldBe 4
  }

}
