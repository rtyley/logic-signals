package com.madgag.logic.fileformat.saleae.csv

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.madgag.logic.fileformat.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.{ChannelMapping, TimeParser}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.StringWriter
import scala.io.Source

class SaleaeCsvTest extends AnyFlatSpec with should.Matchers with Inspectors {
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
  }

}
