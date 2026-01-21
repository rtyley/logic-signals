package com.madgag.logic.signals.validation.timingviolations

import cats.*
import cats.data.*
import cats.syntax.all.*
import com.madgag.logic.signals.selectors.{IntervalSelector, PulseSelector}
import com.madgag.logic.signals.triggers.ChannelGroup
import com.madgag.logic.signals.triggers.Criterion.Timing
import com.madgag.logic.signals.validation.SignalValidation.TransitionPair
import com.madgag.logic.{ChannelSignals, Time}

case class TimingConstraint(name: String, timing: Timing)

/**
 * Failures to adhere to required setup-and-hold times are known as 'violations'.
 *
 * - [[https://www.designnews.com/testing-measurement/how-to-track-down-setup-and-hold-violations-with-a-mixed-signal-oscilloscope]]
 * - [[https://www.tek.com/en/documents/application-note/identifying-setup-and-hold-violations-mixed-signal-oscilloscope]]
 * - [[https://nandland.com/lesson-12-setup-and-hold-time/]]
 */
type Violations[T, C] = Map[TimingConstraint, NonEmptySeq[TransitionPair[T, C]]]

trait ViolationFinder[C] {
  def violationsIn[T: Time](signals: ChannelSignals[T, C]): Violations[T, C]
}

object ViolationFinder {

  def within[C](intervalSelector: IntervalSelector[C])(finders: ViolationFinder[C]*): ViolationFinder[C] = new ViolationFinder[C] {
    override def violationsIn[T: Time](signals: ChannelSignals[T, C]): Violations[T, C] = Monoid.combineAll {
      for {
        pulseSignals <- intervalSelector.selectIn(signals)
        finder <- finders
      } yield finder.violationsIn(pulseSignals)
    }
  }

  def on[C](criteriaSubject: ChannelGroup[C])(pulseCriteria: ChannelGroup[C] => ViolationFinder[C]*): ViolationFinder[C] = new ViolationFinder[C] {
    override def violationsIn[T: Time](signals: ChannelSignals[T, C]): Violations[T, C] =
      pulseCriteria.map(_(criteriaSubject).violationsIn(signals)).combineAll
  }

  def negativePulse[C](criteria: ViolationFinder[C]*): ChannelGroup[C] => ViolationFinder[C] =
    cs => within(cs.negativePulse)(criteria *)

  def positivePulse[C](criteria: ViolationFinder[C]*): ChannelGroup[C] => ViolationFinder[C] =
    cs => within(cs.positivePulse)(criteria *)
}
