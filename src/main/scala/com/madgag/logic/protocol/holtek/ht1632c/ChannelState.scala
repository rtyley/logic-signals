package com.madgag.logic.protocol.holtek.ht1632c

import Channel.*
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.Read
import com.madgag.logic.ChannelMapping
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite

import scala.jdk.CollectionConverters.*


sealed trait Channel
object Channel {
  object Data extends Channel

  sealed trait Clock(val rw: ReadOrWrite) extends Channel

  object Clock {
    object Read extends Clock(ReadOrWrite.Read)
    object Write extends Clock(ReadOrWrite.Write)
  }

  sealed trait ChipSelect extends Channel {
    val index: Int
  }
  object ChipSelect {
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

  def time[T](block: => T): T = {
    val before = System.nanoTime
    val result = block
    val after = System.nanoTime
    println("Elapsed time: " + (after - before) / 1000000 + "ms")
    result
  }
}