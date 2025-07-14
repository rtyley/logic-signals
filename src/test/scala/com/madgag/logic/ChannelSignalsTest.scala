package com.madgag.logic

import com.madgag.logic.Time.Delta
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration
import java.time.Duration.ofMillis
import scala.collection.immutable.SortedMap

class ChannelSignalsTest extends AnyFlatSpec with should.Matchers {
  "ChannelSignals" should "be a thing" in {

    val timeAndStates: SortedMap[Delta, Map[String, Boolean]] = SortedMap(
      ofMillis(1) -> Map("D7" -> true, "D3" -> true, "D1" -> true),
      ofMillis(2) -> Map("D7" -> false, "D3" -> false, "D1" -> false),
      ofMillis(7) -> Map("D7" -> true, "D3" -> true, "D1" -> true),
      ofMillis(8) -> Map("D7" -> false, "D3" -> false, "D1" -> false),
    )

    val channelSignals = ChannelSignals.from(timeAndStates)

    channelSignals.chunksWhile("D7", true) should have size 2
    channelSignals.chunksWhile("D3", true) should have size 2
    channelSignals.chunksWhile("D1", true) should have size 2
  }
}
