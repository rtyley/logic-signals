package com.madgag.logic.fileformat.gusmanb

import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.CapitalisedPickle.ReadWriter
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.{CaptureChannel, TriggerType}

import java.time.Duration
import java.time.Duration.ofSeconds

object GusmanBConfig {
  def read(readable: ujson.Readable, trace: Boolean = false): GusmanBConfig =
    CapitalisedPickle.read[GusmanBConfig](readable, trace)

  /**
   * https://github.com/gusmanb/logicanalyzer/blob/master/Software/LogicAnalyzer/SharedDriver/AnalyzerChannel.cs
   *
   * @param channelNumber surprisingly, this is zero-based, rather than one-based, so GPIO 2 is denoted by
   *                      channelNumber=0, rather than channelNumber=1, despite the one-based channel numbering in this
   *                      diagram:
   *                      https://user-images.githubusercontent.com/4086913/221229250-51e03a76-2e01-48cc-a218-62fd21a8fbfb.png
   */
  case class CaptureChannel(
    channelNumber: Int,
    channelName: String
  ) derives ReadWriter

  enum TriggerType:
    case Edge, Complex, Fast, Blast

  given ReadWriter[TriggerType] = CapitalisedPickle.readwriter[Int].bimap[TriggerType](_.ordinal, TriggerType.fromOrdinal)

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
  triggerType: TriggerType,
  triggerChannel: Int,
  triggerInverted: Boolean,
  triggerBitCount: Int,
  triggerPattern: Int
) derives CapitalisedPickle.ReadWriter {
  val sampleIntervalDuration: Duration = ofSeconds(1).dividedBy(frequency)
  val postTriggerDuration: Duration = sampleIntervalDuration.multipliedBy(postTriggerSamples)
}

