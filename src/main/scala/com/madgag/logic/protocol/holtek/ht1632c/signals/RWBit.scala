package com.madgag.logic.protocol.holtek.ht1632c.signals

import ReadOrWrite.*

case class RWBit(value: Boolean, rw: ReadOrWrite, symbol: String)

object RWBit {

  val bits: Map[ReadOrWrite, Map[Boolean, RWBit]] = Map(
    Read -> Map(false -> RWBit(false, Read, "⓪"), true -> RWBit(true, Read, "①")),
    Write -> Map(false -> RWBit(false, Write, "0"), true -> RWBit(true, Write, "1"))
  )

  def apply(value: Boolean, rw: ReadOrWrite): RWBit = bits(rw)(value)
}