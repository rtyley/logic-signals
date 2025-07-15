package com.madgag.logic

import com.madgag.logic.Isomorphisms.*
import com.madgag.logic.Time.Delta
import com.madgag.logic.TimeParser.TimeFormat

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