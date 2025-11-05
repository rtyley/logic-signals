package com.madgag.logic.protocol.holtek.ht1632c.operations

import cats.kernel.Order.*
import com.madgag.logic.Time.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.Clock
import com.madgag.logic.protocol.holtek.ht1632c.SignalValidation.ValidationResult
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits.Parser
import com.madgag.logic.protocol.holtek.ht1632c.signals.{MixedBits, RWBit}
import com.madgag.logic.{Signal, Time}
import spire.math.Interval
import spire.math.interval.ValueBound

/**
 * Contains the relevant line signals (Read & Write clock, and Data line) sent while a Chip Select (CS) line
 * is held low for transmission of an [[Operation]] (ie [[CommandMode]], [[WriteMode]] or [[ReadMode]])
 */
case class OperationSignals[T: Time](readWriteClocks: Map[Clock, Signal[T]], data: Signal[T]) {

  val interval: Interval[T] = (readWriteClocks.values.toSeq :+ data).map(_.interval).reduce(_ | _)
  val startTime: T = interval.lowerBound.asInstanceOf[ValueBound[T]].a
  
  lazy val mixedBits: Seq[RWBit] = (for {
    (clock, clockSignal) <- readWriteClocks
    time <- clockSignal.goingTo(true)
  } yield time -> clock.rw.bit(data.state(time))).toSeq.sortBy(_._1).map(_._2)

  /**
   * Only one operation can occur per Chip-Select reset - in the Holtek 1632C docs, this is made clearest
   * in the diagram labelled "Mode - Data and Command Mode", but in text the docs do say:
   *
   * ''While the system is operating in the non-successive command or the non-successive address data mode,
   * the CS pin should be set to "1" and the previous operation mode will be reset also. Once the CS
   * pin returns to "0", a new operation mode ID should be issued first.''
   *
   * The Command operation supports multiple commands, and the Read & Write operations support successive
   * addressing, but you can't issue multiple distinct  operations (with the appropriate opening 3-bit
   * command code) without resetting the Chip-Select line.
   */
  lazy val operation: Option[Operation] = summon[Parser[Operation]].parse(mixedBits).map(_._1)

  lazy val opOrBadBits: Either[String, Operation] = operation.toRight(mixedBits.map(_.symbol).mkString)

  //val timingValidation: ValidationResult[Unit] = 
}
