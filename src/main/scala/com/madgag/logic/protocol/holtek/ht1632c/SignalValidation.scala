package com.madgag.logic.protocol.holtek.ht1632c

import cats.data.ValidatedNec
import com.madgag.logic.{ChannelSignals, Event, Time}
import com.madgag.logic.Time.{add, orderForTime}
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.{ReadMode, WriteMode}
import spire.math.Interval
import spire.math.Interval.{atOrAbove, atOrBelow}

import java.time.Duration
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.DurationConverters.*

case class Timing(
  range: Interval[Duration],
  typical: Option[Duration] = None
)

/**
 * From the Holtek HT1632C docs on "A.C. Characteristics":
 */
object Timing {
  def apply(min: FiniteDuration): Timing = Timing(atOrAbove(min.toJava))
  def apply(min: FiniteDuration, typical: FiniteDuration): Timing =
    Timing(atOrAbove(min.toJava), typical = Some(typical.toJava))

  val tCS = Timing(min = 250.nanos)
  val tCLK = Map(ReadMode -> Timing(min = 500.nanos), WriteMode -> Timing(min = 1000.nanos))

  val tsu = Timing(min = 50.nanos, typical = 100.nanos)
  val th = Timing(min = 100.nanos, typical = 200.nanos)
  val tsu1 = Timing(min = 200.nanos, typical = 300.nanos)
  val th1 = th
  val tod = Timing(atOrBelow(200.nanos.toJava), typical = Some(100.nanos.toJava))

  sealed trait TimingError(timing: Timing)


  ChipSelect.positivePulse takes (min = 250.nanos)
  ChipSelect.falling -> Clock.falling takes (min = 50.nanos, typical = 100.nanos)

  Data.change -> Clock.Write.rising takes (min = 200.nanos, typical = 300.nanos)
  Clock.Write.rising -> Data.change takes (min = 100.nanos, typical = 200.nanos)

  Clock.Read.falling -> Data.change takesAtMost (max = 200.nanos, typical = 100.nanos)
  Clock.Read.pulse takes (min = 500.nanos)

  Clock.rising -> ChipSelect.rising takes(min = 100.nanos, typical = 200.nanos)

}

object SignalValidation {
  case class TransitionPair[T, C](start: Event[T, (C, Boolean)], end: Event[T, (C, Boolean)])(using time: Time[T]) {
    val interveningTime: Duration = time.between(start.time, end.time)

    require(!interveningTime.isNegative)
  }

  type ValidationResult[A] = ValidatedNec[String, A]

//  def validate[T, C](channelSignals: ChannelSignals[T, C], criteria: Set[Criterion[C]])(using time: Time[T]): Unit = {
//    for {
//      criterion <- criteria
//      (startChannel, startSignal) <- channelSignals.data if criterion.startAndEnd._1.criteriaTrigger.appliesTo(startChannel)
//      (endChannel, endSignal) <- channelSignals.data if criterion.startAndEnd._2.criteriaTrigger.appliesTo(endChannel)
//      startEvent <- startSignal.events() if criterion.startAndEnd._1.goingTo.contains(startEvent.value)
//      endSignalFromStartingEventOnwards = endSignal.subInterval(atOrAbove(startEvent.time))
//      subsequentEndEvent <- endSignal.events().find(e => criterion.startAndEnd._2.goingTo.contains(e.value))
//      interveningTime = time.between(startEvent.time, subsequentEndEvent.time)
//      if !criterion.timing.range.contains(interveningTime)
//    } yield (criterion, startEvent, subsequentEndEvent)
//  }

  def validate[T, C](channelSignals: ChannelSignals[T, C], criterion: Criterion[C])(using time: Time[T]): Seq[TransitionPair[T, C]] = {
    (for {
      (startChannel, startSignal) <- channelSignals.data if criterion.start.criteriaTrigger.appliesTo(startChannel)
      (endChannel, endSignal) <- channelSignals.data if criterion.end.criteriaTrigger.appliesTo(endChannel)
      startEvent <- startSignal.events() if criterion.start.goingTo.contains(startEvent.value)
      endSignalFromStartingEventOnwards = endSignal.subInterval(atOrAbove(startEvent.time))
      subsequentEndEvent <- endSignalFromStartingEventOnwards.events().find(e => criterion.end.goingTo.contains(e.value))
      transitionPair = TransitionPair(startEvent.map(startChannel -> _), subsequentEndEvent.map(endChannel -> _))
      if !criterion.timing.range.contains(transitionPair.interveningTime)
    } yield transitionPair).toSeq
  }
}
