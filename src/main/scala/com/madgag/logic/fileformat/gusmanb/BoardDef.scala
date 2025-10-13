package com.madgag.logic.fileformat.gusmanb

case class BoardDef(captureBufferSize: Int) {
  def maxSamplesFor(captureMode: CaptureMode): Int = captureBufferSize / captureMode.bufferSizeFactor
}

object BoardDef {
  val MaxFrequency = 100000000
  val MaxChannels = 24 // TODO

  val Pico = BoardDef(
    captureBufferSize = 128 * 1024, // https://github.com/gusmanb/logicanalyzer/blob/39a7a0b43fa2de03927cc17ce397b2fc4fbe314b/Firmware/LogicAnalyzer_V2/LogicAnalyzer_Board_Settings.h#L59
  )

  val Pico2 = BoardDef(
    captureBufferSize = 128 * 3 * 1024, // https://github.com/gusmanb/logicanalyzer/blob/39a7a0b43fa2de03927cc17ce397b2fc4fbe314b/Firmware/LogicAnalyzer_V2/LogicAnalyzer_Board_Settings.h#L79C38-L79C52
  )
}
