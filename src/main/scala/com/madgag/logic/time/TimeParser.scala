package com.madgag.logic.time

import com.madgag.logic.Isomorphisms.*
import com.madgag.logic.time.Time
import com.madgag.logic.time.Time.Delta
import com.madgag.logic.time.TimeParser.TimeFormat

import java.lang.Math.round
import java.time.Duration.ofNanos

object TimeParser {
  type TimeFormat[T] = String <=> T
  
  val DeltaParser: TimeParser[Delta] = TimeParser[Delta]("Time [s]", new TimeFormat[Delta] {
    override val to = (str: String) => ofNanos(round(str.toDouble * 1e9))
    override val from = (d: Delta) => f"${d.toNanos.toDouble / 1e9}%1.9f"
  })
}

case class TimeParser[T: Time](fieldName: String, timeFormat: TimeFormat[T])