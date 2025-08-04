package com.madgag.logic.fileformat.gusmanb

import com.madgag.logic.GpioPin
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel.AllChannels
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration
import java.time.Duration.ofNanos

class GusmanBConfigTest extends AnyFlatSpec with should.Matchers with OptionValues {
  "GusmanB config" should "be readable" in {
    val config: GusmanBConfig = GusmanBConfig.read(os.read(os.resource / "ht-capture.tcs"))
    config.frequency shouldBe 100_000_000
    config.sampleIntervalDuration shouldBe ofNanos(10)
  }

  it should "have the right channels" in {
    val channels = AllChannels
    channels.head.displayText shouldBe "CH 1"
    channels.last.displayText shouldBe "CH 24"
  }

  it should "Fly" in {
    val gpio = GusmanBConfig.Channel.AllAvailableGpioPins
    gpio.head shouldBe GpioPin(2)
    gpio.last shouldBe GpioPin(28)

    val channel22 = AllChannels.find(_.displayText == "CH 22").value
    channel22.zeroBased shouldBe 21
    channel22.gpioPin shouldBe GpioPin(26)

    gpio should contain (GpioPin(26))
  }
}