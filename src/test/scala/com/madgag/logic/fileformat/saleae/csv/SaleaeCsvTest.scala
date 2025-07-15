package com.madgag.logic.fileformat.saleae.csv

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.madgag.logic.Time.Delta
import com.madgag.logic.fileformat.*
import com.madgag.logic.fileformat.Record.csvReaderForResource
import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.{ChannelMapping, ChannelSignals, TimeParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.StringWriter
import scala.io.Source

class SaleaeCsvTest extends AnyFlatSpec with should.Matchers {
  "Saleae CSV export format" should "round-trip" in {
    def summarise(csvText: String): Unit = {
      val lines = csvText.linesIterator.toSeq
      println(s"start:\n${lines.take(3).mkString("\n")}")
      println(s"end:\n${lines.takeRight(3).mkString("\n")}")
    }

    val channelMapping = ChannelMapping[Channel](
      "Chip Select" -> ChipSelect.Follower.One,
      "What dat" -> ChipSelect.Leader,
      "Write" -> Clock.Write,
      "Data" -> Data
    )
    val format: CSVLogicFormat[Delta, Channel] = SaleaeCsv.format(TimeParser.DeltaParser, channelMapping)

    val original = Source.fromResource("saleae-export.csv").mkString
    summarise(original)

    val signals = Foo.read(format)(CSVReader.open(Source.fromString(original)))

    println(s"signals.interval=${signals.interval.mapBounds(TimeParser.DeltaParser.timeFormat.from)}")
    println(s"signals.changeTimes.last=${signals.changeTimes.last}")

    val fields: Seq[String] = TimeParser.DeltaParser.fieldName +: channelMapping.fieldsInPreferredOrder.map(_._1)
    val writer = new StringWriter()
    Foo.write(signals, CSVDetails(CSVHeader(fields), format))(CSVWriter.open(writer))
    val stuff = writer.toString
    summarise(stuff)
  }

}
