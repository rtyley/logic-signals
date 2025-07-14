package com.madgag.logic.fileformat

import com.github.tototoshi.csv.CSVReader
import com.madgag.logic.protocol.holtek.ht1632c.ChannelState
import com.madgag.logic.protocol.holtek.ht1632c.ChannelState.timeField

import java.io.{BufferedReader, InputStreamReader}
import java.lang.Math.round
import java.time.Duration
import java.time.Duration.ofNanos
import scala.math.Ordering.Implicits.*

object Record {

  def csvReaderForResource(resourcePath: String): CSVReader =
    CSVReader.open(new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(resourcePath))))
}
