package com.madgag.logic.signals.validation

import cats.data.ValidatedNec
import com.gu.time.duration.formatting.*
import com.madgag.logic.Event
import com.madgag.logic.signals.triggers.Criterion.Trigger.MatchAttributes
import com.madgag.logic.time.Time

import java.time.Duration

object SignalValidation {
  case class TransitionPair[T, C](start: Event[T, MatchAttributes], end: Event[T, MatchAttributes])(using time: Time[T]) {
    val interveningTime: Duration = time.between(start.time, end.time)

    require(!interveningTime.isNegative)

    val summary: String = s"${start.value}→${end.value}: ${interveningTime.format()} @ ${end.time}"
  }

  type ValidationResult[A] = ValidatedNec[String, A]
}
