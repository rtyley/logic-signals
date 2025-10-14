package com.madgag.logic.fileformat.gusmanb

import cats.data.NonEmptySet
import cats.kernel.Order
import com.madgag.logic.GpioPin
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.CapitalisedPickle.ReadWriter
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel.{AllChannels, CH_16, CH_5}
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Trigger.TriggerType
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.{CaptureChannel, Channel, Trigger}
import GusmanBConfig.*
import scodec.bits.BitVector
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

    lazy val gpioPin: GpioPin = GpioPin(displayNumber + (if (displayNumber <= 21 ) 1 else 4))

    def offsetBy(offset: Int): Option[Channel] = AllChannels.find(_.zeroBased == zeroBased + offset)

    override def compare(that: Channel): Int = zeroBased.compare(that.zeroBased)
  }

  object Channel {
    val AllChannels: SortedSet[Channel] = SortedSet.from((0 to 23).map(Channel(_)))

    val ChannelByDisplayNumber: Map[Int, Channel] = AllChannels.map(ch => ch.displayNumber -> ch).toMap

    val CH_1: Channel = ChannelByDisplayNumber(1)
    val CH_5: Channel = ChannelByDisplayNumber(5)
    val CH_8: Channel = ChannelByDisplayNumber(8)
    val CH_9: Channel = ChannelByDisplayNumber(9)
    val CH_16: Channel = ChannelByDisplayNumber(16)
    val CH_17: Channel = ChannelByDisplayNumber(17)
    val CH_24: Channel = ChannelByDisplayNumber(24)

    val ChannelsByGpioPin: SortedMap[GpioPin, Channel] =
      SortedMap.from(AllChannels.map(channel => channel.gpioPin -> channel))

    val AllAvailableGpioPins: SortedSet[GpioPin] = ChannelsByGpioPin.keySet

    given Order[Channel] = Order.fromComparable
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
    triggerChannel: Channel, // triggerChannel is independent of whatever is being captured
    triggerInverted: Option[Boolean] = None,
    triggerBitCount: Option[Int] = None,
    triggerPattern: Option[Int] = None // triggerPattern is on consecutive channels, based on triggerChannel onwards
  ) {
    lazy val finalChannelOfPattern: Option[Channel] = triggerBitCount.flatMap(triggerChannel.offsetBy)

    require(triggerType.highestPermittedChannelWithinPattern.forall(_ >= finalChannelOfPattern.get))
  }

  object Trigger {
    enum TriggerType(val highestPermittedChannelWithinPattern: Option[Channel] = None):
      case Edge extends TriggerType()
      case Complex extends TriggerType(highestPermittedChannelWithinPattern = Some(CH_16))
      case Fast extends TriggerType(highestPermittedChannelWithinPattern = Some(CH_5))
      case Blast extends TriggerType() // https://github.com/gusmanb/logicanalyzer/issues/218#issuecomment-2723117313

    object TriggerType:
      def optimalTypeFor(highestChannelWithinPattern: Channel): Option[TriggerType] =
        Seq(Fast, Complex).find(_.highestPermittedChannelWithinPattern.forall(_ >= highestChannelWithinPattern))

    def withOptimalTypeForPattern(bits: BitVector, baseChannel: Channel): Option[Trigger] = for {
      highestChannelWithinPattern <- baseChannel.offsetBy(bits.length.toInt)
      typ <- TriggerType.optimalTypeFor(highestChannelWithinPattern)
    } yield Trigger(typ, baseChannel, triggerBitCount = bits.intSize, triggerPattern = Some(bits.toInt(signed = false)))

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
  val channels: NonEmptySet[Channel] = NonEmptySet.fromSetUnsafe(SortedSet.from(captureChannels.map(_.channelNumber)))
  
  val sampleIntervalDuration: Duration = ofSeconds(1).dividedBy(frequency)
  val postTriggerDuration: Duration = sampleIntervalDuration.multipliedBy(postTriggerSamples)
  val captureMode: CaptureMode = CaptureMode.forChannels(channels)
  
  def issueWithBoard(board: BoardDef): Option[SamplingIssue] = SamplingIssue.of(board, channels, totalSamples)
}
case class SamplingIssue(maxSamples: Int, requestedSamples: Int, captureMode: CaptureMode) {
  val summary: String = s"$captureMode only permits $maxSamples - requested $requestedSamples"
}

object SamplingIssue {
  def of(boardDef: BoardDef, channels: NonEmptySet[Channel], requestedSamples: Int): Option[SamplingIssue] = {
    val captureMode = CaptureMode.forChannels(channels)
    val maxSamples = boardDef.maxSamplesFor(captureMode)
    Option.when(requestedSamples > maxSamples)(SamplingIssue(
      maxSamples, requestedSamples, captureMode
    ))
  }
}