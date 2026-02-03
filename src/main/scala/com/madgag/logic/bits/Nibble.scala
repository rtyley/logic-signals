package com.madgag.logic.bits

import com.madgag.logic.BitEndian
import com.madgag.logic.BitEndian.{BigFirst, LittleFirst}
import scodec.bits.BitVector

case class Nibble(bitVector: BitVector) {
  require(bitVector.size == 4)

  def apply(index: Int, bitEndian: BitEndian): Boolean = bitVector(bitEndian match {
    case BigFirst => bitVector.size-1 - index
    case LittleFirst => index
  })
}

object Nibble {
  val All: IndexedSeq[Nibble] = (0 to 15).map(i => Nibble(BitVector.fromInt(i).takeRight(4)))
}