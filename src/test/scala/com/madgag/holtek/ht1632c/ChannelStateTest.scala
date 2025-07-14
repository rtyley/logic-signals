package com.madgag.holtek.ht1632c

import com.madgag.logic.protocol.holtek.ht1632c.HoltekBits.{operationSignalsFor, operationsFor}
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.Write
import com.madgag.logic.Time.Delta
import com.madgag.logic.{ChannelMapping, ChannelSignals, Signal, TestKit, TimeParser}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spire.math.interval.ValueBound
import com.madgag.logic.Time.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.protocol.holtek.ht1632c.{Channel, ChipLed, HoltekBits}
import com.madgag.logic.protocol.holtek.ht1632c.operations.{Operation, OperationSignals, WriteMode}

import java.time.Duration.{ofMillis, ofNanos}
import scala.collection.immutable.SortedMap
import com.madgag.scala.collection.decorators.*

import java.time.Duration
import java.time.temporal.ChronoUnit
import scala.math.Ordering.Implicits.*

class ChannelStateTest extends AnyFlatSpec with should.Matchers with OptionValues {

  val starts101ChannelMapping = ChannelMapping[Channel](
    "What dat" -> ChipSelect.Leader,
    "Chip Select" -> ChipSelect.Follower.One,
    "Write" -> Clock.Write,
    "Data" -> Data
  )

  "Holtek 101 Write" should "be a thing" in {
    val boom = HoltekBits.loadResource("/digital.starts-101.csv", TimeParser.DeltaParser, starts101ChannelMapping)
    operationSignalsFor(boom, ChipSelect.Follower.One).head.operation.value shouldBe a[WriteMode]
  }

  "Pico cap" should "finally read like ascending integers" in {
    val channelMapping = ChannelMapping[Channel](
      "Red LED - CS" -> ChipSelect.Leader,
      "Blue LED - Write" -> Clock.Write,
      "Out pin - Data" -> Data
    )

    val boom = HoltekBits.loadResource("/digital.pico-cap.csv", TimeParser.DeltaParser, channelMapping)
    println(operationSignalsFor(boom, ChipSelect.Leader).head.mixedBits.map(_.symbol).mkString)
  }


  val dualChannelMapping = ChannelMapping[Channel](
    "Green LED" -> ChipSelect.Follower.One,
    "Red LED - CS" -> ChipSelect.Leader,
    "Blue LED - Write" -> Clock.Write,
    "Out pin - Data" -> Data
  )

  it should "do dual HT1632C drivin" in {
    val boom = HoltekBits.loadResource("/digital.pico.init-commands-and-write-single-led.csv", TimeParser.DeltaParser, dualChannelMapping)
    for (chipSelect <- Seq(ChipSelect.Leader, ChipSelect.Follower.One))
      val boo: OperationSignals[Delta] = operationSignalsFor(boom, chipSelect).head
      println(boo.mixedBits.map(_.symbol).mkString)

    val opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]] = operationsFor(boom.transform(_.deglitch(ofNanos(200))))

    println(opsByChip)
  }


  it should "handle the simple case" in {
    val signals = TestKit.signals(
      ChipSelect.Leader -> "█▁▁▁▁▁▁▁▁▁▁▁",
      Clock.Write       -> "███▁██▁██▁██",
      Data              -> "▁▁███▁▁▁████"
    )
    signals.splitOn(ChipSelect.Leader, goingToValue = false).head.data(Clock.Write).goingTo(true) should have size 3
  }

  "Big" should "be fine" in {
    val boom = HoltekBits.loadResource("/saleae-export.csv", TimeParser.DeltaParser, starts101ChannelMapping)
    val opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]] = operationsFor(boom.transform(_.deglitch(ofNanos(200))))

    println(opsByChip)

    pook(opsByChip)

    println(HoltekBits.commandsFrom(opsByChip).mkString("\n"))

  }

  private def pook(opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]]): Unit = {
    val ledSignals: ChannelSignals[Delta, ChipLed] = HoltekBits.ledStatesFromWriteSignalsIn(opsByChip)
    val chipLedToLitTimes =
      ledSignals.data.mapV(_.intervalsWhile(true).map(_.mapBounds(_.truncatedTo(ChronoUnit.MILLIS)))).toSeq.filter(_._2.nonEmpty).sortBy(x => x._2.headOption.map(interval => interval.lowerBound.asInstanceOf[ValueBound[Delta]].a -> interval.duration) -> x._1)
    val oneSecondWonders = chipLedToLitTimes.filter(_._2.exists(interval => interval.duration > ofMillis(900) && interval.duration < ofMillis(1100)))

    println(oneSecondWonders.mkString("\n"))
  }

//  it should "handle gusmanb" in {
//    // Data,Write,CS1,CS2
//    val starts101ChannelMapping = ChannelMapping[Channel](Map(
//      "Data" -> Data,
//      "Write" -> Clock.Write,
//      "CS1" -> ChipSelect.Leader,
//      "CS2" -> ChipSelect.Follower.One
//    ))
//    val boom = HoltekBits.loadGusman("/gusmanb-logicanalyzer-capture.csv", Duration.ofNanos(10), starts101ChannelMapping)
//    val opsByChip: Map[ChipSelect, SortedMap[Delta, Operation]] = operationsFor(boom.transform(_.deglitch(ofNanos(200))))
//    println(opsByChip)
//    pook(opsByChip)
//  }
}

