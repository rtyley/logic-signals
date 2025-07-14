package com.madgag.logic.protocol.holtek.ht1632c

import cats.kernel.Order.*
import Channel.ChipSelect
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.Write
import com.madgag.logic.Time.*
import com.madgag.logic.protocol.holtek.ht1632c.operations.WriteMode
import com.madgag.logic.{ChannelSignals, Time}
import com.madgag.scala.collection.decorators.*
import scodec.bits.BitVector

import scala.collection.immutable.SortedMap

/**
 * Alternatively, lets play WriteModes directly into Event[Boolean] across the hundred or so channels, then
 * convert them to Signal objects which are easier to analyse.
 */
case class State(memoryMap: Vector[BitVector]) {
  def applyWrite(writeMode: WriteMode): State = State(writeMode.grouped(Write).foldLeft(memoryMap) {
    case (acc, (index, chunk)) => acc.patch(index, chunk.map(_.value), chunk.size)
  })
}

case class ChipLed(chipSelect: ChipSelect, ledAddress: LedAddress)
object ChipLed {
  given Ordering[ChipLed] = Ordering.by(cl => (cl.chipSelect, cl.ledAddress))
}

object State {

  /**
   * Map[T, WriteMode] depends on us having enough time resolution to always distinguish being WriteMode instants
   */
  def signalsByLed[T: Time](writesByTime: SortedMap[T, WriteMode]): ChannelSignals[T, LedAddress] =
    ChannelSignals.from(SortedMap.from(writesByTime.mapV(_.writesByLedAddress)))
}