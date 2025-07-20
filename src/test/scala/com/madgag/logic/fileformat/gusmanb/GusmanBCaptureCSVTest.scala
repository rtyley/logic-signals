package com.madgag.logic.fileformat.gusmanb

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.madgag.logic.fileformat.Foo
import com.madgag.logic.fileformat.saleae.csv.SaleaeCsv
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.{ChannelMapping, TimeParser}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.StringWriter
import java.time.Duration
import scala.io.Source


class GusmanBCaptureCSVTest extends AnyFlatSpec with should.Matchers with Inspectors {
  "GusmanB CSV export format" should "round-trip" in {
    // Data,Write,CS1,CS2

    val csvDetails = GusmanBCaptureCSV.csvDetails(Duration.ofMillis(1), ChannelMapping(
      "Data" -> Data,
      "Write" -> Clock.Write,
      "CS1" -> ChipSelect.Leader,
      "CS2" -> ChipSelect.Follower.One
    ))

    val original = Source.fromResource("gusmanb-logicanalyzer-capture.csv").mkString
    // summarise(original)

    val signals = Foo.read(csvDetails.format)(CSVReader.open(Source.fromString(original)))
    println(signals)

    val writer = new StringWriter()
    Foo.write(signals, csvDetails)(CSVWriter.open(writer)(SaleaeCsv.CsvFormat))
    val stuff = writer.toString

    val orgLines = original.linesWithSeparators.toSeq
    val stuLines = stuff.linesWithSeparators.toSeq
    stuLines.size shouldEqual orgLines.size

    forAll(orgLines.zip(stuLines)) { (org, stu) =>
      stu shouldEqual org
    }
    println(stuff)
    stuff shouldEqual original
  }
}
