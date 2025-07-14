package com.madgag.logic.fileformat.saleae.csv

import com.github.tototoshi.csv.CSVWriter
import com.madgag.logic.Time.Delta
import com.madgag.logic.fileformat.*
import com.madgag.logic.fileformat.Record.csvReaderForResource
import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.{ChannelMapping, ChannelSignals, TimeParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.StringWriter

class SaleaeCsvTest extends AnyFlatSpec with should.Matchers {
  "Saleae CSV export format" should "round-trip" in {
    val channelMapping = ChannelMapping[Channel](
      "Chip Select" -> ChipSelect.Follower.One,
      "What dat" -> ChipSelect.Leader,
      "Write" -> Clock.Write,
      "Data" -> Data
    )
    val format: CSVLogicFormat[Delta, Channel] = SaleaeCsv.format(channelMapping, TimeParser.DeltaParser)

    val bunk: ChannelSignals[Delta, Channel] = Foo.read(format)(csvReaderForResource("/saleae-export.csv"))

    val fields: Seq[String] =
      TimeParser.DeltaParser.fieldName +: channelMapping.fieldsInPreferredOrder.map(_._1)
    val writer = new StringWriter()
    val csvDetails = CSVDetails(CSVHeader(fields), format)
    Foo.write(bunk, csvDetails)(CSVWriter.open(writer))
    val stuff = writer.toString
    println(s"stuff=$stuff")
  }

}
