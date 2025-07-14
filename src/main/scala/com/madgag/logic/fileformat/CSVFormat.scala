package com.madgag.logic.fileformat

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.madgag.logic.Isomorphisms.<=>
import com.madgag.logic.{ChannelSignals, Time}

import scala.util.Using

type CSVLogicFormat[T, C] = ChannelSignals[T, C] <=> LazyList[Map[String, String]]

case class CSVDetails[T, C](header: CSVHeader, format: CSVLogicFormat[T, C])

object Foo {
  def write[T: Time, C](
    signals: ChannelSignals[T, C],
    csvDetails: CSVDetails[T, C]
  )(writer: CSVWriter): Unit = Using.resource(writer) { w =>
    val headerFields = csvDetails.header.fields
    w.writeRow(headerFields)
    w.writeAll(csvDetails.format.to(signals).map(fieldValuesByName => headerFields.map(fieldValuesByName)))
  }

  def read[T: Time, C](format: CSVLogicFormat[T, C])(
    reader: CSVReader
  ): ChannelSignals[T, C] = format.from(reader.toLazyListWithHeaders())
}