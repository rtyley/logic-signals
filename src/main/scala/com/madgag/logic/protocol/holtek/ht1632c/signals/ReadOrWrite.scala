package com.madgag.logic.protocol.holtek.ht1632c.signals

enum ReadOrWrite:
  case Read
  case Write
  
  def bit(v: Boolean): RWBit = RWBit(v, this)
