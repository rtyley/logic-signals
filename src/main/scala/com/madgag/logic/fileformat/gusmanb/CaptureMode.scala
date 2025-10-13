package com.madgag.logic.fileformat.gusmanb

import cats.data.*
import cats.implicits.*
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel
import com.madgag.logic.fileformat.gusmanb.GusmanBConfig.Channel.{CH_16, CH_24, CH_8}

enum CaptureMode(val highestPermittedChannel: Channel, val bufferSizeFactor: Int):
  case MODE_8_CHANNEL extends CaptureMode(CH_8, 1)
  case MODE_16_CHANNEL extends CaptureMode(CH_16, 2)
  case MODE_24_CHANNEL extends CaptureMode(CH_24, 4)

object CaptureMode {
  def forChannels(channels: NonEmptySet[Channel]): CaptureMode = {
    val highestChannel = channels.toSortedSet.last
    CaptureMode.values.find(_.highestPermittedChannel >= highestChannel).get
  }
}