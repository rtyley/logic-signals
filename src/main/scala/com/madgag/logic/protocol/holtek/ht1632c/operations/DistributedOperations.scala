package com.madgag.logic.protocol.holtek.ht1632c.operations

import cats.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.ChipSelect
import com.madgag.logic.protocol.holtek.ht1632c.operations.Operation
import com.madgag.logic.time.TimedF

case class DistributedOperations[B[_]](ops: Seq[B[(ChipSelect, Operation)]]) {
  def mapK[D[_]](fk: B ~> D)(using Functor[B]) =
    DistributedOperations[D](ops.map { csWithOp => fk(csWithOp) })
}

type UntimedDistributedOperations = DistributedOperations[Id]
type TimedDistributedOperations[T] = DistributedOperations[TimedF[T]]
