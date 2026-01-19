package com.madgag.logic.protocol.holtek.ht1632c

import com.madgag.logic.protocol.holtek.ht1632c.Channel.*
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite
import com.madgag.logic.signals.triggers.ChannelGroup

sealed trait Channel extends ChannelGroup[Channel] {
  override def includes[C1 >: Channel](channel: C1): Boolean = channel == this
}

object Channel {
  case object Data extends Channel

  sealed trait Clock(val rw: ReadOrWrite) extends Channel

  case object Clock extends ChannelGroup[Channel] {
    case object Read extends Clock(ReadOrWrite.Read)
    case object Write extends Clock(ReadOrWrite.Write)

    override def includes[C1 >: Channel](channel: C1): Boolean = channel.isInstanceOf[Clock]
  }

  sealed trait ChipSelect extends Channel {
    val index: Int
  }
  case object ChipSelect extends ChannelGroup[Channel] {
    override def includes[C1 >: Channel](channel: C1): Boolean = channel.isInstanceOf[ChipSelect]

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
}