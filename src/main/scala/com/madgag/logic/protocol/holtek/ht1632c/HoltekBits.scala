package com.madgag.logic.protocol.holtek.ht1632c

import cats.kernel.Order.*
import com.madgag.logic.Time.*
import com.madgag.logic.fileformat.Foo
import com.madgag.logic.fileformat.Record.csvReaderForResource
import com.madgag.logic.fileformat.saleae.csv.SaleaeCsv
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock}
import com.madgag.logic.protocol.holtek.ht1632c.operations.*
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.WriteMode
import com.madgag.logic.protocol.holtek.ht1632c.signals.TimingCharacteristics
import com.madgag.logic.{ChannelMapping, ChannelSignals, Time, TimeParser}

import java.time.Duration
import java.time.Duration.ofNanos
import scala.collection.immutable.SortedMap

object HoltekBits {

  def loadResource[T: Time](name: String, timeParser: TimeParser[T], channelMapping: ChannelMapping[Channel], deglitchTime: Duration): ChannelSignals[T, Channel] = {
    val deglitchedSignals = {
      val channelSignals = Foo.read(SaleaeCsv.format(timeParser, channelMapping))(csvReaderForResource(name))
      channelSignals.transform(_.deglitch(deglitchTime))
    }
    val anomaliesByCriterion = TimingCharacteristics.violationFinder.violationsIn(deglitchedSignals)
    println(anomaliesByCriterion.map {
      case (key, value) => key.name + ":\n\t" + value.toSeq.take(3).map(_.summary).mkString(", ")
    }.mkString("\n"))
    require(anomaliesByCriterion.isEmpty)
    deglitchedSignals
  }

  def operationSignalsFor[T: Time](
    channelSignals: ChannelSignals[T, Channel],
    chipSelect: ChipSelect
  ): Iterable[OperationSignals[T]] = for {
    chunk <- channelSignals.chunksWhile(chipSelect, false) if !chunk.isConstant
  } yield OperationSignals(
    readWriteClocks = chunk.data.collect { case (c: Clock, s) => c -> s },
    data = chunk.data(Channel.Data)
  )

  def opsFor[T: Time](channelSignals: ChannelSignals[T, Channel]): Seq[(ChipSelect, OperationSignals[T])] = for {
    chipSelectChannel <- channelSignals.data.keys.toSeq.collect { case cs: ChipSelect => cs }
    opSignal <- operationSignalsFor(channelSignals, chipSelectChannel)
  } yield chipSelectChannel -> opSignal

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
