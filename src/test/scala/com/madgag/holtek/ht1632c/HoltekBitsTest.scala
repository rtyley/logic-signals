package com.madgag.holtek.ht1632c

import cats.*
import cats.implicits.*
import com.madgag.logic.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.protocol.holtek.ht1632c.operations.*
import com.madgag.logic.protocol.holtek.ht1632c.{Channel, ChipLed, HoltekBits}
import com.madgag.logic.time.Time.*
import com.madgag.logic.time.TimedF.given
import com.madgag.logic.time.{Time, TimeParser, Timed, TimedF}
import com.madgag.scala.collection.decorators.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration
import java.time.Duration.{ofMillis, ofSeconds}

class HoltekBitsTest extends AnyFlatSpec with should.Matchers with OptionValues {
  val ginClockChannelMapping = ChannelMapping[Channel](
    "CS Leader" -> ChipSelect.Leader,
    "CS Follower" -> ChipSelect.Follower.One,
    "Write" -> Clock.Write,
    "Data" -> Data
  )

  "Holtek 101 Write" should "be a thing" in {
    val signals =
      HoltekBits.loadResource("/gin-clock.large-sample.saleae-export.csv",
        TimeParser.DeltaParser,
        ginClockChannelMapping,
        deglitchTime = Duration.ofNanos(2000) // Seen a legit gin clock pulse length 3840 nanos
      )

    pook(HoltekBits.operationsFor(signals).flatTraverseChipVal(_.operation))
  }

  private def pook[T: Time](chipSeq: ChipSeq[Timed[T, Operation]]): Unit = {
    val opsAfterAllCommandsFinished = 
      chipSeq.reverse.takeWhile(!_.value._2.isInstanceOf[CommandMode]).reverse

    val ledSignals: ChannelSignals[T, ChipLed] = HoltekBits.ledStatesFromWriteSignalsIn(opsAfterAllCommandsFinished)

    val chipLedToLitTimes: Seq[(ChipLed, Iterable[BoundedInterval[T]])] =
      ledSignals.data.mapV(_.intervalsWhile(true)).toSeq.filter(_._2.nonEmpty)
        .sortBy(x => x._2.headOption.map(interval => interval.lowerValueBound.a -> interval.duration) -> x._1)
    val oneSecondWonders = chipLedToLitTimes.filter(_._2.exists(_.duration.minus(ofSeconds(1)).abs < ofMillis(100)))

    // println(oneSecondWonders.mkString("\n"))
  }
}

