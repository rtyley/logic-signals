package com.madgag.logic.fileformat.gusmanb

import com.madgag.logic.Time.{*, given}
import com.madgag.logic.fileformat.{CSVDetails, CSVHeader, CSVLogicFormat}
import com.madgag.logic.{ChannelMapping, ChannelSignals, Event, Time}

import java.time.Duration

object GusmanBCaptureCSV {

  def csvDetails[Channel](
    rowDuration: Duration,
    channelMapping: ChannelMapping[Channel]
  ): CSVDetails[Delta, Channel] = {
    val fields: Seq[String] = channelMapping.fieldsInPreferredOrder.map(_._1)
    CSVDetails(CSVHeader(fields), format(rowDuration, channelMapping))
  }
  
  def format[Channel](
    rowDuration: Duration,
    channelMapping: ChannelMapping[Channel]
  ): CSVLogicFormat[Delta, Channel] = new CSVLogicFormat[Delta, Channel] {

    override val to = (signals: ChannelSignals[Delta, Channel]) => for (
      rowTime <- signals.interval.lazyList(rowDuration)
    ) yield channelMapping.csvFieldsFor(signals.at(rowTime))

    override val from = (rows: LazyList[Map[String, String]]) => channelMapping.signals(
      for ((row, index) <- rows.zipWithIndex) yield Event(rowDuration.multipliedBy(index), channelMapping.parseState(row))
    )
  }
}
