package com.madgag.logic.signals.selectors

import com.madgag.logic.Signal.ChangeType
import com.madgag.logic.signals.triggers.ChannelGroup
import com.madgag.logic.{ChannelSignals, Time}

/**
 * Examples:
 *
 * pulse: (Trigger[C], Trigger[C]) = change -> change
 * positivePulse: (Trigger[C], Trigger[C]) = rising -> falling
 * negativePulse: (Trigger[C], Trigger[C]) = falling -> rising
 *
 * ...for those examples, we would want the instance of C to be the same for the start & end
 * Those are Pulses - not the more general case which can involve two different channels.
 * [[PairSelector]] can handle those.
 *
 * Start -> Clock.Write.rising
 * Clock.Write.rising -> End
 * Start -> Data.change
 *
 * ...here the channel is completely different, we _know_ the instance of C will be different
 *
 * Conceivably, we might also want things like 'Clock.Write.rising -> Data.change' but pulse seems
 * to handle those cases right now.
 */
case class PulseSelector[+C](channelGroup: ChannelGroup[C], pulseType: ChangeType) {
  def selectIn[T: Time, C1 >: C](signals: ChannelSignals[T, C1]): Iterable[ChannelSignals[T, C1]] = for {
    (channel, signal) <- signals.data if channelGroup.includes(channel)
    (interval, state) <- signal.intervals() if pulseType.goingTo(state)
  } yield signals.unsafeSubInterval(interval)
}
