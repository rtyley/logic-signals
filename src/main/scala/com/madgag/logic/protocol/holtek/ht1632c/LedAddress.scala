package com.madgag.logic.protocol.holtek.ht1632c

case class LedAddress(memoryAddress: Int, dataIndex: Int) extends Ordered[LedAddress] {
  require(dataIndex >= 0 && dataIndex < 4)

  val ledIndex: Int = (memoryAddress * 4) + dataIndex

  override def compare(that: LedAddress): Int = ledIndex - that.ledIndex
}
