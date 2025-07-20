package com.madgag.logic.fileformat.gusmanb

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration
import java.time.Duration.ofNanos

class GusmanBConfigTest extends AnyFlatSpec with should.Matchers {
  "GusmanB config" should "be readable" in {
    val config: GusmanBConfig = GusmanBConfig.read(os.read(os.resource / "ht-capture.tcs"))
    config.frequency shouldBe 100_000_000
    config.sampleIntervalDuration shouldBe ofNanos(10)
  }
}