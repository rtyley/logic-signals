package com.madgag.logic.protocol.holtek.ht1632c

import cats.kernel.Order.*
import com.madgag.logic.Time.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock}
import com.madgag.logic.protocol.holtek.ht1632c.operations.*
import com.madgag.logic.{ChannelMapping, ChannelSignals, Time, TimeParser}

import scala.collection.immutable.SortedMap

object HoltekBits {

  def loadResource[T: Time](name: String, timeParser: TimeParser[T], channelMapping: ChannelMapping[Channel]): ChannelSignals[T, Channel] = {
    val burma = ??? // rowsForResource(name).map(channelMapping.parse(_, timeParser))
//    println(s"Got ${burma.size}")
    channelMapping.signals(burma)
  }

//  def loadGusman(name: String, rowDuration: Duration, channelMapping: ChannelMapping[Channel]): ChannelSignals[Duration, Channel] =
//    GusmanBCaptureCSV.parse(rowsForResource(name), rowDuration, channelMapping)

  def operationSignalsFor[T: Time](channelSignals: ChannelSignals[T, Channel], chipSelect: ChipSelect): Iterable[OperationSignals[T]] = {

    for {
      chunk <- channelSignals.chunksWhile(chipSelect, false) if !chunk.isConstant
    } yield {
      val clockSignals = chunk.data.collect { case (c: Clock, s) => c -> s }
      OperationSignals(clockSignals, chunk.data(Channel.Data))
    }
  }

  def operationsFor[T: Time](channelSignals: ChannelSignals[T, Channel]): Map[ChipSelect, SortedMap[T, Operation]] = {
    // println(s"Got channelSignals: $channelSignals")
    val chipSelectChannels = channelSignals.data.keySet.collect { case cs: ChipSelect => cs }
    (
      for {
        chipSelectChannel <- chipSelectChannels
      } yield {
        val opsByTime = SortedMap.from(for {
          opSignal <- operationSignalsFor(channelSignals, chipSelectChannel)
          op <- opSignal.operation.toSeq
        } yield opSignal.startTime -> op)
        chipSelectChannel -> opsByTime
      }
    ).toMap
  }
  
  def ledStatesFromWriteSignalsIn[T: Time](opsByChip: Map[ChipSelect, SortedMap[T, Operation]]): ChannelSignals[T, ChipLed] = (for {
    (chip, opsByTime) <- opsByChip
  } yield State.signalsByLed(opsByTime.collect { case (time, w: WriteMode) => time -> w }).mapKeys(la => ChipLed(chip, la))).reduce(_ merge _)


  def commandsFrom[T: Time](opsByChip: Map[ChipSelect, SortedMap[T, Operation]]): SortedMap[T, (ChipSelect, Seq[Command])] = (for {
    (chip, opsByTime) <- opsByChip
  } yield opsByTime.collect { case (time, c: CommandMode) => time -> (chip, c.commands) }).reduce(_ ++ _)

}
