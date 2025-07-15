package com.madgag.logic.fileformat.saleae.csv

import com.madgag.logic.fileformat.CSVLogicFormat
import com.madgag.logic.{ChannelMapping, ChannelSignals, Time, TimeParser}

object SaleaeCsv {

  def format[T: Time, Channel](
    timeParser: TimeParser[T],
    channelMapping: ChannelMapping[Channel]
  ): CSVLogicFormat[T, Channel] = new CSVLogicFormat[T, Channel] {

    override def to = (signals: ChannelSignals[T, Channel]) =>
      for (rowTime <- LazyList.from(signals.changeAndBoundTimes)) yield {
        channelMapping.csvFieldsFor(signals.at(rowTime)) + (timeParser.fieldName -> timeParser.timeFormat.from(rowTime))
      }

    override def from = (rows: LazyList[Map[String, String]]) =>
      channelMapping.signals(rows.map(row => channelMapping.parse(row, timeParser)))
  }
}
