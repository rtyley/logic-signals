package com.madgag.logic.signals.triggers

import com.madgag.logic.Signal.ChangeType.*
import com.madgag.logic.signals.selectors.PulseSelector
import com.madgag.logic.signals.triggers.Criterion.SignalChange

trait ChannelGroup[+C] {
  def includes[C1 >: C](channel: C1): Boolean

  val change = SignalChange[C](this, Change)
  val rising = SignalChange[C](this, Rising)
  val falling = SignalChange[C](this, Falling)

  val pulse: PulseSelector[C] = PulseSelector(this, Change)
  val positivePulse: PulseSelector[C] = PulseSelector(this, Rising)
  val negativePulse: PulseSelector[C] = PulseSelector(this, Falling)
}
