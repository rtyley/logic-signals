package com.madgag.logic.fileformat.gusmanb

import cats.data.*
import com.madgag.logic.fileformat.gusmanb.CaptureMode.{MODE_16_CHANNEL, MODE_24_CHANNEL}
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CaptureModeTest extends AnyFlatSpec with should.Matchers with OptionValues {
  "CaptureMode.forChannels" should "be correct" in {
    CaptureMode.forChannels(NonEmptySet.of(CH_24)) shouldBe MODE_24_CHANNEL
    CaptureMode.forChannels(NonEmptySet.of(CH_17)) shouldBe MODE_24_CHANNEL

    CaptureMode.forChannels(NonEmptySet.of(CH_1, CH_17)) shouldBe MODE_24_CHANNEL

    CaptureMode.forChannels(NonEmptySet.of(CH_16)) shouldBe MODE_16_CHANNEL
    CaptureMode.forChannels(NonEmptySet.of(CH_1, CH_16)) shouldBe MODE_16_CHANNEL
  }
}
