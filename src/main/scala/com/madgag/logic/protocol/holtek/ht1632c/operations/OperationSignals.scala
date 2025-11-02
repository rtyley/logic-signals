package com.madgag.logic.protocol.holtek.ht1632c.operations

import cats.kernel.Order.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Clock
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits.Parser
import com.madgag.logic.Time.*
import com.madgag.logic.protocol.holtek.ht1632c.signals.{MixedBits, RWBit, ReadOrWrite}
import com.madgag.logic.{Signal, Time}
import spire.math.Interval
import spire.math.interval.ValueBound

import scala.collection.immutable.SortedMap

case class OperationSignals[T: Time](readWriteClocks: Map[Clock, Signal[T]], data: Signal[T]) {

  val interval: Interval[T] = (readWriteClocks.values.toSeq :+ data).map(_.interval).reduce(_ | _)
  val startTime: T = interval.lowerBound.asInstanceOf[ValueBound[T]].a
  
  lazy val mixedBits: Seq[RWBit] = (for {
    (clock, clockSignal) <- readWriteClocks
    time <- clockSignal.goingTo(true)
  } yield time -> clock.rw.bit(data.state(time))).toSeq.sortBy(_._1).map(_._2)

  lazy val operation: Option[Operation] = summon[Parser[Operation]].parse(mixedBits).map(_._1)

  lazy val opOrBadBits: Either[String, Operation] = operation.toRight(mixedBits.map(_.symbol).mkString)
}
