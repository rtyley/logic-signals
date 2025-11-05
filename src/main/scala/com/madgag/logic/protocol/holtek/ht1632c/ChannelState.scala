package com.madgag.logic.protocol.holtek.ht1632c

import com.madgag.logic.Time
import com.madgag.logic.Time.orderForTime
import com.madgag.logic.protocol.holtek.ht1632c.Channel.*
import com.madgag.logic.protocol.holtek.ht1632c.CriteriaTrigger.Event
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite
import spire.math.Interval
import spire.math.Interval.{atOrAbove, atOrBelow}

import scala.concurrent.duration.FiniteDuration
import scala.jdk.DurationConverters.*

trait CriteriaTrigger[C] {
  val change = CriteriaTrigger.Event(this, Set(false, true))
  val rising = CriteriaTrigger.Event(this, Set(true))
  val falling = CriteriaTrigger.Event(this, Set(false))

  val pulse: (Event[C], Event[C]) = change -> change
  val positivePulse: (Event[C], Event[C]) = rising -> falling

  val appliesTo: C => Boolean
}

case class Criterion[C](startAndEnd: (Event[C], Event[C]), timing: Timing) {
  val (start, end) = startAndEnd
}

object CriteriaTrigger {
  case class Event[C](criteriaTrigger: CriteriaTrigger[C], goingTo: Set[Boolean])

  extension [C] (fromTo: (Event[C], Event[C]))
    def has(timing: Timing) = Criterion(fromTo, timing)

    def takes(min: FiniteDuration) = Criterion(fromTo, Timing(atOrAbove(min.toJava)))
    def takes(min: FiniteDuration, typical: FiniteDuration) =
      Criterion(fromTo, Timing(atOrAbove(min.toJava), Some(typical.toJava)))
    def takesAtMost(max: FiniteDuration, typical: FiniteDuration) =
      Criterion(fromTo, Timing(atOrBelow(max.toJava), Some(typical.toJava)))
}

sealed trait Channel extends CriteriaTrigger[Channel] {
  override val appliesTo: Channel => Boolean = _ == this
}

object Channel {
  case object Data extends Channel

  sealed trait Clock(val rw: ReadOrWrite) extends Channel

  case object Clock extends CriteriaTrigger {
    case object Read extends Clock(ReadOrWrite.Read)
    case object Write extends Clock(ReadOrWrite.Write)

    override val appliesTo: Channel => Boolean = _.isInstanceOf[Clock]
  }

  sealed trait ChipSelect extends Channel {
    val index: Int
  }
  case object ChipSelect extends CriteriaTrigger {
    override val appliesTo: Channel => Boolean = _.isInstanceOf[ChipSelect]

    given Ordering[ChipSelect] = Ordering.by(_.index)
    
    case object Leader extends ChipSelect {
      val index: Int = 0
    }
    case class Follower(index: Int) extends ChipSelect {
      require(index > 0)
    }
    object Follower {
      val One: Follower = Follower(1)
    }
  }
}

object ChannelState {

  val timeField = "Time [s]"
//
//  def time[T](block: => T): T = {
//    val before = System.nanoTime
//    val result = block
//    val after = System.nanoTime
//    println("Elapsed time: " + (after - before) / 1000000 + "ms")
//    result
//  }
}