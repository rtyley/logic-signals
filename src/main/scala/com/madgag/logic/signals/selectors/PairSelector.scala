package com.madgag.logic.signals.selectors

import com.madgag.logic.Time.*
import com.madgag.logic.signals.triggers.Criterion.Trigger
import com.madgag.logic.signals.validation.SignalValidation.TransitionPair
import com.madgag.logic.{ChannelSignals, Time}
import spire.math.Interval.atOrAbove


case class PairSelector[C](start: Trigger[C], end: Trigger[C]) {

  // If we made this return Option[TransitionPair[T, C]], this might cover all our use-cases and
  // force non-multiplicity discipline
  // Possibly it should be Either[Error, TransitionPair[T, C]], to allow us to denote when multiple are found,
  // given that we only expect one.
  def selectIn[T: Time](signals: ChannelSignals[T, C]): Iterable[TransitionPair[T, C]] = for {
    startEvent <- start.occurrencesIn(signals)
    signalsFollowingStart = signals.unsafeSubInterval(atOrAbove(startEvent.time))
    endEvent <- end.occurrencesIn(signalsFollowingStart)
      .filter(e => !(e.time == startEvent.time && e.value == startEvent.value)) // still necessary if we have PulseSelectors?
  } yield TransitionPair(startEvent, endEvent)
}
