package com.madgag.logic.fileformat.saleae.csv

import com.github.tototoshi.csv.{CSVFormat, DefaultCSVFormat}
import com.madgag.logic.fileformat.{CSVDetails, CSVHeader, CSVLogicFormat}
import com.madgag.logic.time.{Time, TimeParser}
import com.madgag.logic.{ChannelMapping, ChannelSignals}

object SaleaeCsv {
  val CsvFormat: CSVFormat = new DefaultCSVFormat {
    override val lineTerminator: String = "\n"
  }
  
  def csvDetails[T: Time, Channel](
    timeParser: TimeParser[T],
    channelMapping: ChannelMapping[Channel]
  ): CSVDetails[T, Channel] = {
    val fields: Seq[String] = TimeParser.DeltaParser.fieldName +: channelMapping.fieldsInPreferredOrder.map(_._1)
    CSVDetails(CSVHeader(fields), format(timeParser, channelMapping))
  }

  def format[T: Time, Channel](
    timeParser: TimeParser[T],
    channelMapping: ChannelMapping[Channel]
  ): CSVLogicFormat[T, Channel] = new CSVLogicFormat[T, Channel] {

    override def to: ChannelSignals[T, Channel] => LazyList[Map[String, String]] = signals =>
      for (rowTime <- LazyList.from(signals.changeAndBoundTimes)) yield {
        channelMapping.csvFieldsFor(signals.at(rowTime)) + (timeParser.fieldName -> timeParser.timeFormat.from(rowTime))
      }

    override def from: LazyList[Map[String, String]] => ChannelSignals[T, Channel] = rows =>
      channelMapping.signals(rows.map(row => channelMapping.parse(row, timeParser)))
  }
}
