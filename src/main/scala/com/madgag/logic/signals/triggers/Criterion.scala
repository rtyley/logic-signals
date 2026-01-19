package com.madgag.logic.signals.triggers

import cats.data.NonEmptySeq
import com.gu.time.duration.formatting.*
import com.gu.time.duration.formatting.intervals.DurationRangeFormatter
import com.madgag.logic.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.Direction.{Asc, Desc}
import com.madgag.logic.Signal.ChangeType
import com.madgag.logic.Time.*
import com.madgag.logic.signals.selectors.PairSelector
import com.madgag.logic.signals.triggers.ChannelGroup
import com.madgag.logic.signals.triggers.Criterion.Trigger.Bound.IntervalToBound
import com.madgag.logic.signals.triggers.Criterion.Trigger.MatchAttributes
import com.madgag.logic.signals.validation.timingviolations.{TimingConstraint, ViolationFinder, Violations}
import spire.math.Interval
import spire.math.Interval.{atOrAbove, atOrBelow}
import spire.math.interval.ValueBound

import java.time.Duration
import scala.concurrent.duration.FiniteDuration
import scala.jdk.DurationConverters.ScalaDurationOps

//case class Criterion[C](startAndEnd: (Trigger[C], Trigger[C]), timing: Timing) {
//  val (start, end) = startAndEnd
//
//  def unacceptableEndTimesGiven[T: Time](startTime: T): Seq[Interval[T]] =
//    (~timing.range.mapBounds(startTime.add)).map(_ & atOrAbove(startTime))
//
//  def validate[T: Time](channelSignals: ChannelSignals[T, C]): Seq[TransitionPair[T, C]] = (for {
//    startEvent <- start.occurrencesIn(channelSignals)
//    unacceptableEventInterval: Interval[T] <- unacceptableEndTimesGiven(startEvent.time)
//    endEvent <- end.occurrencesIn(channelSignals.unsafeSubInterval(unacceptableEventInterval))
//      .filter(e => !(e.time == startEvent.time && (e.value -- startEvent.value).isEmpty))
//  } yield TransitionPair(startEvent, endEvent)).toSeq
//
//  val summary: String = {
//    val subjectConditionsSummary =
//      s"${start.summary}-${Option.when(start.subject != end.subject)(end.subject).mkString}${end.change.char}"
//
//    s"$subjectConditionsSummary ${rangeFormatter(timing.range)}"
//  }
//}

object Criterion {
  private val rangeFormatter = DurationRangeFormatter.from(_.format())

  case class Timing(range: Interval[Duration], typical: Option[Duration] = None) {
    def allows(duration: Duration): Boolean = range.contains(duration)
  }

  /**
   * A trigger may be, for instance, the start or end of the signals interval, or the first/last occurrence
   * of a [[SignalChange]] in the signals.
   *
   * Technically, a trigger may occur multiple times in signal interval, but to reduce work done,
   * we would want to avoid using triggers like that as much as possible.
   *
   * @tparam C the type of all channels
   */
  trait Trigger[+C] {
    def occurrencesIn[T: Time, C1 >: C](signals: ChannelSignals[T, C1]): Iterable[Event[T, MatchAttributes]]

    def summary: String
  }
  
  object Trigger {
    object Bound {
      type IntervalToBound[T] = BoundedInterval[T] => ValueBound[T]
    }

    trait MatchAttributes

    case class MatchingChannels[C](matchingChannels: Set[C]) extends MatchAttributes

    sealed abstract class Bounds extends Trigger[Nothing] with MatchAttributes {
      def bound[T]: IntervalToBound[T]

      override def occurrencesIn[T: Time, D >: Nothing](signals: ChannelSignals[T, D]): Iterable[Event[T, MatchAttributes]] =
        Iterable.single(Event(bound(signals.interval).a, this))

      def summary: String = this.getClass.getSimpleName
    }

    case object Start extends Bounds {
      def bound[T]: IntervalToBound[T] = _.lowerValueBound
    }

    case object End extends Bounds {
      def bound[T]: IntervalToBound[T] = _.upperValueBound
    }

    private def initial[C](signalChange: SignalChange[C], direction: Direction): Trigger[C] = new Trigger[C] {
      def occurrencesIn[T: Time, C1 >: C](signals: ChannelSignals[T, C1]): Iterable[Event[T, MatchAttributes]] =
        signals.eventsFor(signalChange, direction).headOption

      val summary: String = s"${direction.fold("first", "last")} ${signalChange.summary}"
    }

    def first[C](signalChange: SignalChange[C]): Trigger[C] = initial(signalChange, Asc)
    def last[C](signalChange: SignalChange[C]): Trigger[C] = initial(signalChange, Desc)
  }

  case class SignalChange[+C](channelGroup: ChannelGroup[C], change: ChangeType) {
    def triggeredByValueOf(e: com.madgag.logic.Event[_, Boolean]): Boolean = change.goingTo.contains(e.value)

    val summary = s"$channelGroup${change.char}"
  }

  extension [C](fromTo: (Trigger[C], Trigger[C]))
    def has(constraint: TimingConstraint): ViolationFinder[C] = new ViolationFinder[C] {
      override def violationsIn[T: Time](signals: ChannelSignals[T, C]): Violations[T, C] = {
        val (from, to) = fromTo
        val pairSelector = PairSelector(from, to)

        Map.from(
          NonEmptySeq.fromSeq(pairSelector.selectIn(signals)
            .filter(pair => !constraint.timing.allows(pair.interveningTime)).toSeq).map(constraint -> _).toSeq
        )
      }
    }

    def takes(name: String, min: FiniteDuration) = has(TimingConstraint(name, Timing(atOrAbove(min.toJava))))
    def takes(name: String, min: FiniteDuration, typical: FiniteDuration) =
      has(TimingConstraint(name, Timing(atOrAbove(min.toJava), Some(typical.toJava))))
    def takesAtMost(name: String, max: FiniteDuration, typical: FiniteDuration) =
      has(TimingConstraint(name, Timing(atOrBelow(max.toJava), Some(typical.toJava))))
}
