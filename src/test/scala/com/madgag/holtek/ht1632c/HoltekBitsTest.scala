package com.madgag.holtek.ht1632c

import cats.*
import cats.syntax.foldable.*
import com.madgag.logic.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.protocol.holtek.ht1632c.HoltekBits.operationSignalsFor
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.COM.DisplayLayout.`32x8`
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.COM.OpenDrain.NMOS
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.OffOn.{Off, On}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.Switchable.{Blink, LedDutyCycleGenerator, SystemOscillator}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.SyncRole.RCLeader
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.{COM, PWM}
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.WriteMode
import com.madgag.logic.protocol.holtek.ht1632c.operations.{Command, CommandMode, DistributedOperations, Operation, TimedDistributedOperations}
import com.madgag.logic.protocol.holtek.ht1632c.{Channel, ChipLed, HoltekBits, LedAddress}
import com.madgag.logic.time.Time.*
import com.madgag.logic.time.TimedF.given
import com.madgag.logic.time.{Time, TimeParser, TimedF}
import com.madgag.scala.collection.decorators.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spire.implicits.*
import spire.math.interval.ValueBound

import java.time.Duration
import java.time.Duration.ofMillis

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

    pook(HoltekBits.operationsFor(signals))
  }

  private def pook[T: Time](opsByChip: TimedDistributedOperations[T]): Unit = {
    val opsByChip2 = DistributedOperations(
      opsByChip.ops.reverse.takeWhile(!_.value._2.isInstanceOf[CommandMode]).reverse
    )

    val ledSignals: ChannelSignals[T, ChipLed] = HoltekBits.ledStatesFromWriteSignalsIn(opsByChip2)

    val ledsThatGetLit = ChannelSignals(ledSignals.data.filter(_._2.intervalsWhile(true).nonEmpty))

    println(ledsThatGetLit.summary)
    val chipLeds = ledsThatGetLit.data.keys
    println(chipLeds.groupUp(_.chipSelect) {
      chipLeds => val leds = chipLeds.map(_.ledAddress) ; (leds.min, leds.max)
    })

    val chipLedToLitTimes =
      ledSignals.data.mapV(_.intervalsWhile(true)).toSeq.filter(_._2.nonEmpty).sortBy(x => x._2.headOption.map(interval => interval.lowerBound.asInstanceOf[ValueBound[Delta]].a -> interval.duration) -> x._1)
    val oneSecondWonders = chipLedToLitTimes.filter(_._2.exists(interval => {
      val dur: Duration = interval.duration
      (dur > ofMillis(900)) && (dur < ofMillis(1100))
    }))

    // println(oneSecondWonders.mkString("\n"))
  }
}

