package com.madgag.logic.protocol.holtek.ht1632c.signals

import com.madgag.logic.protocol.holtek.ht1632c.Channel
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock, Data}
import com.madgag.logic.signals.triggers.Criterion
import com.madgag.logic.signals.triggers.Criterion.Trigger.*
import com.madgag.logic.signals.triggers.Criterion.takes
import com.madgag.logic.signals.validation.SignalValidation
import com.madgag.logic.signals.validation.timingviolations.ViolationFinder
import com.madgag.logic.signals.validation.timingviolations.ViolationFinder.{negativePulse, on, positivePulse, within}

import scala.concurrent.duration.*

/**
 * From the Holtek HT1632C docs on "A.C. Characteristics":
 *
 * [[https://cdn-shop.adafruit.com/datasheets/ht1632cv120.pdf]]
 * [[https://github.com/user-attachments/assets/5da53fb2-314c-4b04-9ee6-ee66cb053a8c]]
 */
object TimingCharacteristics {

  val tod: ViolationFinder[Channel] = within(Clock.Read.negativePulse)(
    Start -> first(Data.change) // should really be oneAtMost
      takesAtMost("tod", max = 200.nanos, typical = 100.nanos)
  )

  val dataSetupAndHoldAroundClock: ViolationFinder[Channel] = within(Data.pulse)( // 'between' might be better for Data.change -> Data.change
    Start -> first(Clock.rising) takes("tsu", min = 50.nanos, typical = 100.nanos),
    last(Clock.rising) -> End takes("th", min = 100.nanos, typical = 200.nanos)
  )

  val violationFinder: ViolationFinder[Channel] =
    on(ChipSelect)(
      positivePulse(Start -> End takes ("tCS", min = 250.nanos)),
      negativePulse(
        Start -> first(Clock.falling) takes("tsu1", min = 200.nanos, typical = 300.nanos),
        last(Clock.rising) -> End takes("th1", min = 100.nanos, typical = 200.nanos),
        within(first(Clock.falling) -> last(Clock.rising))(
          within(Clock.Read.pulse)(Start -> End takes("tCLK_Read", min = 1000.nanos)),
          within(Clock.Write.pulse)(Start -> End takes("tCLK_Write", min = 500.nanos)),
          dataSetupAndHoldAroundClock,
          tod,
        )
      )
    )
}
