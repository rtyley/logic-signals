package com.madgag.logic.fileformat.saleae.csv

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.madgag.logic.*
import com.madgag.logic.fileformat.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.time.Time.Delta
import com.madgag.logic.time.TimeParser
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.StringWriter
import java.time.Duration.ofSeconds
import scala.io.Source

class SaleaeCsvTest extends AnyFlatSpec with should.Matchers with Inspectors {

  "Saleae CSV export format" should "round-trip on a real small sample" in {
    val csvDetails = SaleaeCsv.csvDetails(TimeParser.DeltaParser, ChannelMapping[Channel](
      "CS" -> ChipSelect.Leader,
      "WR" -> Clock.Write,
      "DA" -> Data
    ))

    val signals = ChannelSignals[Delta, Channel](Map(
      ChipSelect.Leader -> Signal.forIntervalImpliedBy(Seq(
        Event(ofSeconds(5), true),
        Event(ofSeconds(7), false),
        Event(ofSeconds(18), true),
        Event(ofSeconds(20), true)
      )),
      Clock.Write -> Signal.forIntervalImpliedBy(Seq(
        Event(ofSeconds(5), true),
        Event(ofSeconds(8), false),
        Event(ofSeconds(10), true),
        Event(ofSeconds(12), false),
        Event(ofSeconds(17), true),
        Event(ofSeconds(20), true)
      )),
      Data -> Signal.forIntervalImpliedBy(Seq(
        Event(ofSeconds(5), true),
        Event(ofSeconds(8), false),
        Event(ofSeconds(9), true),
        Event(ofSeconds(12), false),
        Event(ofSeconds(17), true),
        Event(ofSeconds(20), true)
      )),
    ))

    val writer = new StringWriter()
    Foo.write(signals, csvDetails)(CSVWriter.open(writer)(SaleaeCsv.CsvFormat))
    val stuff = writer.toString

    println(stuff)

    val recoveredChannelSignals: ChannelSignals[Delta, Channel] =
      Foo.read(csvDetails.format)(CSVReader.open(Source.fromString(stuff)))
    recoveredChannelSignals shouldBe signals
  }

  "Saleae CSV export format" should "round-trip" in {
    def summarise(csvText: String): Unit = {
      println(s"csvText.length=${csvText.length}")
      val lines = csvText.linesIterator.toSeq
      println(s"start:\n${lines.take(3).mkString("\n")}")
      println(s"end:\n${lines.takeRight(3).mkString("\n")}")
    }

    val csvDetails = SaleaeCsv.csvDetails(TimeParser.DeltaParser, ChannelMapping(
      "Chip Select" -> ChipSelect.Follower.One,
      "What dat" -> ChipSelect.Leader,
      "Write" -> Clock.Write,
      "Data" -> Data
    ))

    val original = Source.fromResource("saleae-export.csv").mkString
    summarise(original)

    val signals = Foo.read(csvDetails.format)(CSVReader.open(Source.fromString(original)))

    println(s"signals.interval=${signals.interval.mapBounds(TimeParser.DeltaParser.timeFormat.from)}")
    println(s"signals.changeTimes.last=${signals.changeTimes.last}")

    val writer = new StringWriter()
    Foo.write(signals, csvDetails)(CSVWriter.open(writer)(SaleaeCsv.CsvFormat))
    val stuff = writer.toString
    summarise(stuff)

    val orgLines = original.linesWithSeparators.toSeq
    val stuLines = stuff.linesWithSeparators.toSeq
    stuLines.size shouldEqual orgLines.size

    forAll (orgLines.zip(stuLines)) { (org, stu) =>
      stu shouldEqual org
    }
    stuff shouldEqual original
    println("Finished really")
  }

}
