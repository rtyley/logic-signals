package com.madgag.logic

import com.madgag.logic.TestKit.CharDuration
import com.madgag.logic.Time.Delta
import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration.ofMillis
import scala.collection.immutable.SortedMap

class ChannelSignalsTest extends AnyFlatSpec with should.Matchers {
  given CharDuration = CharDuration(ofMillis(1))
  
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

  "splitOn()" should "allow chopping multiple signals into trigger-based chunks" in {
    val signals: ChannelSignals[Delta, Channel] = TestKit.signals(
      ChipSelect.Leader -> "█▁▁▁▁▁▁▁▁▁▁▁█▁▁▁▁▁",
      Clock.Write       -> "███▁██▁██▁█████▁██"
    )
    val chunks = signals.splitOn(ChipSelect.Leader, goingToValue = false).toSeq

    chunks(0).data(Clock.Write).goingTo(true) should have size 3
    chunks(1).data(Clock.Write).goingTo(true) should have size 1
  }
}
