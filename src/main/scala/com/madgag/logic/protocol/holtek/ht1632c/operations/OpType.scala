package com.madgag.logic.protocol.holtek.ht1632c.operations

import com.madgag.logic.BitEndian.BigFirst
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.{ReadMode, WriteMode}
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits.Parser
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.Write
import scodec.*
import scodec.bits.*

enum OpType(val code: BitVector, val parser: Parser[Operation]):
  case DataRead extends OpType(bin"110", summon[Parser[ReadMode]])
  case DataWrite extends OpType(bin"101", summon[Parser[WriteMode]])
  case Command extends OpType(bin"100", summon[Parser[CommandMode]])

object OpType {
  def from(bits: BitVector): Option[OpType] = OpType.values.find(opType => bits.startsWith(opType.code))

  given MixedBits.Parser[OpType] =
    Parser.extract(3, BigFirst, Write).flatMap(opBits => Parser.opt(OpType.from(opBits)))
}