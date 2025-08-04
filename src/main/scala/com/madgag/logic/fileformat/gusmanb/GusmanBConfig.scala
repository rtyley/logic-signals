package com.madgag.logic.fileformat.gusmanb

import com.madgag.logic.GpioPin
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.CapitalisedPickle.ReadWriter
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Trigger.TriggerType
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.{CaptureChannel, Trigger}
import upickle.implicits.flatten

import java.time.Duration
import java.time.Duration.ofSeconds
import scala.collection.immutable.{SortedMap, SortedSet}

object GusmanBConfig {
  /**
   * https://github.com/gusmanb/logicanalyzer/blob/master/Software/LogicAnalyzer/SharedDriver/AnalyzerChannel.cs
   * Surprisingly, this is zero-based, rather than one-based, so GPIO 2 is denoted by channelNumber=0, rather than
   * channelNumber=1, despite the one-based channel numbering in this diagram:
   * https://user-images.githubusercontent.com/4086913/221229250-51e03a76-2e01-48cc-a218-62fd21a8fbfb.png
   */
  case class Channel(zeroBased: Int) extends Ordered[Channel] {
    val displayNumber: Int = zeroBased + 1

    val displayText = s"CH $displayNumber" // as seen at https://github.com/gusmanb/logicanalyzer/wiki/02---LogicAnalyzer-Hardware

    lazy val gpioPin: GpioPin = GpioPin(displayNumber + (if (displayNumber <= 22 ) 1 else 4))

    override def compare(that: Channel): Int = zeroBased.compare(that.zeroBased)
  }

  object Channel {
    val AllChannels: SortedSet[Channel] = SortedSet.from((0 to 23).map(Channel(_)))

    val ChannelsByGpioPin: SortedMap[GpioPin, Channel] =
      SortedMap.from(AllChannels.map(channel => channel.gpioPin -> channel))

    val AllAvailableGpioPins: SortedSet[GpioPin] = ChannelsByGpioPin.keySet

    given ReadWriter[Channel] = CapitalisedPickle.readwriter[Int].bimap[Channel](_.zeroBased, Channel(_))
  }

  def read(readable: ujson.Readable, trace: Boolean = false): GusmanBConfig =
    CapitalisedPickle.read[GusmanBConfig](readable, trace)
    
  def write(gusmanBConfig: GusmanBConfig): String =
    CapitalisedPickle.write[GusmanBConfig](gusmanBConfig)

  case class CaptureChannel(
    channelNumber: Channel,
    channelName: String
  ) derives ReadWriter

  case class Trigger (
    triggerType: TriggerType,
    triggerChannel: Channel,
    triggerInverted: Option[Boolean] = None,
    triggerBitCount: Option[Int] = None,
    triggerPattern: Option[Int] = None
  )

  object Trigger {
    enum TriggerType:
      case Edge, Complex, Fast, Blast

    given ReadWriter[TriggerType] = CapitalisedPickle.readwriter[Int].bimap[TriggerType](_.ordinal, TriggerType.fromOrdinal)
  }

  object CapitalisedPickle extends upickle.AttributeTagged {
    override def objectAttributeKeyWriteMap(s: CharSequence): String = s.toString.capitalize

    override def objectAttributeKeyReadMap(s: CharSequence): String = {
      val str = s.toString
      str.head.toLower +: str.tail
    }
  }
}

case class GusmanBConfig(
  frequency: Long,
  preTriggerSamples: Int,
  postTriggerSamples: Int,
  totalSamples: Int,
  captureChannels: Seq[CaptureChannel],
  @flatten trigger: Trigger
) derives CapitalisedPickle.ReadWriter {
  val sampleIntervalDuration: Duration = ofSeconds(1).dividedBy(frequency)
  val postTriggerDuration: Duration = sampleIntervalDuration.multipliedBy(postTriggerSamples)
}
