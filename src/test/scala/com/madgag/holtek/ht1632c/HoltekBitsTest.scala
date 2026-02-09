package com.madgag.holtek.ht1632c

import com.madgag.logic.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.protocol.holtek.ht1632c.HoltekBits.operationSignalsFor
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.COM.DisplayLayout.`32x8`
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.COM.OpenDrain.NMOS
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.OffOn.{Off, On}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.Switchable.{Blink, LedDutyCycleGenerator, SystemOscillator}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.SyncRole.RCLeader
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.{COM, PWM}
import com.madgag.logic.protocol.holtek.ht1632c.operations.{Command, CommandMode, Operation}
import com.madgag.logic.protocol.holtek.ht1632c.{Channel, ChipLed, HoltekBits}
import com.madgag.logic.time.Time.*
import com.madgag.logic.time.TimeParser
import com.madgag.scala.collection.decorators.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spire.math.interval.ValueBound

import java.time.Duration
import java.time.Duration.ofMillis
import scala.collection.immutable.SortedMap
import scala.math.Ordering.Implicits.*

class HoltekBitsTest extends AnyFlatSpec with should.Matchers with OptionValues {
  val ginClockChannelMapping = ChannelMapping[Channel](
    "CS Leader" -> ChipSelect.Leader,
    "CS Follower" -> ChipSelect.Follower.One,
    "Write" -> Clock.Write,
    "Data" -> Data
  )

  "Holtek 101 Write" should "be a thing" in {
    val boom =
      HoltekBits.loadResource("/gin-clock.large-sample.saleae-export.csv",
        TimeParser.DeltaParser,
        ginClockChannelMapping,
        deglitchTime = Duration.ofNanos(2000) // Seen a legit gin clock pulse length 3840 nanos
      )
    val followerOpSignals = operationSignalsFor(boom, ChipSelect.Follower.One)
    val operations = followerOpSignals.map(_.operation.value)
    followerOpSignals.head.operation.value shouldBe a[CommandMode]

    val commands = operations.take(7).flatMap(_.asInstanceOf[CommandMode].commands)

    operations.take(7) shouldBe Seq(
      SystemOscillator(Off),
      COM(NMOS,`32x8`),
      RCLeader,
      SystemOscillator(On),
      PWM(16),
      Blink(Off),
      LedDutyCycleGenerator(Off)
    ).map(CommandMode(_))

    val distOps = HoltekBits.operationsFor(boom)
    println(distOps)
  }



//  "Pico cap" should "finally read like ascending integers" in {
//    val channelMapping = ChannelMapping[Channel](
//      "Red LED - CS" -> ChipSelect.Leader,
//      "Blue LED - Write" -> Clock.Write,
//      "Out pin - Data" -> Data
//    )
//
//    val boom = HoltekBits.loadResource("/digital.pico-cap.csv", TimeParser.DeltaParser, channelMapping)
//    println(operationSignalsFor(boom, ChipSelect.Leader).head.mixedBits.map(_.symbol).mkString)
//  }
//
//
//  val dualChannelMapping = ChannelMapping[Channel](
//    "Green LED" -> ChipSelect.Follower.One,
//    "Red LED - CS" -> ChipSelect.Leader,
//    "Blue LED - Write" -> Clock.Write,
//    "Out pin - Data" -> Data
//  )
//
//  it should "do dual HT1632C drivin" in {
//    val boom = HoltekBits.loadResource("/digital.pico.init-commands-and-write-single-led.csv", TimeParser.DeltaParser, dualChannelMapping)
//    for (chipSelect <- Seq(ChipSelect.Leader, ChipSelect.Follower.One))
//      val boo: OperationSignals[Delta] = operationSignalsFor(boom, chipSelect).head
//      println(boo.mixedBits.map(_.symbol).mkString)
//
//    val opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]] = operationsFor(boom.transform(_.deglitch(ofNanos(200))))
//
//    println(opsByChip)
//  }
//
//  "Big" should "be fine" in {
//    val channelSignals: ChannelSignals[Delta, Channel] = HoltekBits.loadResource("/saleae-export.csv", TimeParser.DeltaParser, starts101ChannelMapping)
//    val opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]] =
//      operationsFor(channelSignals.transform(_.deglitch(ofNanos(200))))
//
//    pook(opsByChip)
//
//    println(HoltekBits.commandsFrom(opsByChip).mkString("\n"))
//  }

  private def pook(opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]]): Unit = {
    val ledSignals: ChannelSignals[Delta, ChipLed] = HoltekBits.ledStatesFromWriteSignalsIn(opsByChip)
    val chipLedToLitTimes =
      ledSignals.data.mapV(_.intervalsWhile(true)).toSeq.filter(_._2.nonEmpty).sortBy(x => x._2.headOption.map(interval => interval.lowerBound.asInstanceOf[ValueBound[Delta]].a -> interval.duration) -> x._1)
    val oneSecondWonders = chipLedToLitTimes.filter(_._2.exists(interval => interval.duration > ofMillis(900) && interval.duration < ofMillis(1100)))

    println(oneSecondWonders.mkString("\n"))
  }
}

