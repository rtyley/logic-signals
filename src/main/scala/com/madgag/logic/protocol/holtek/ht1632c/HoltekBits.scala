package com.madgag.logic.protocol.holtek.ht1632c

import cats.*
import cats.kernel.Order.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.fileformat.Foo
import com.madgag.logic.fileformat.Record.csvReaderForResource
import com.madgag.logic.fileformat.saleae.csv.SaleaeCsv
import com.madgag.logic.protocol.holtek.ht1632c.Channel.{ChipSelect, Clock}
import com.madgag.logic.protocol.holtek.ht1632c.operations.*
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.WriteMode
import com.madgag.logic.protocol.holtek.ht1632c.signals.TimingCharacteristics
import com.madgag.logic.time.Time.*
import com.madgag.logic.time.{Time, TimeParser, Timed, TimedF}
import com.madgag.logic.{BoundedInterval, ChannelMapping, ChannelSignals, toBoundedIntervalOpt}

import java.time.Duration
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
  
  def operationsFor[T: Time](channelSignals: ChannelSignals[T, Channel]): TimedDistributedOperations[T] =
    DistributedOperations[TimedF[T]]((for {
      chipSelectChannel <- channelSignals.data.keySet.collect { case cs: ChipSelect => cs }.toSeq
      opSignal <- operationSignalsFor(channelSignals, chipSelectChannel)
      boundedInterval <- opSignal.interval.toBoundedIntervalOpt.toSeq
      op <- opSignal.operation.toSeq
    } yield Timed(boundedInterval, chipSelectChannel -> op)).sortBy(_.interval.lowerValueBound.a))
  
  def ledStatesFromWriteSignalsIn[T: Time](opsByChip: Map[ChipSelect, SortedMap[T, Operation]]): ChannelSignals[T, ChipLed] = (for {
    (chip, opsByTime) <- opsByChip
  } yield State.signalsByLed(opsByTime.collect { case (time, w: WriteMode) => time -> w }).mapKeys(la => ChipLed(chip, la))).reduce(_ merge _)


  def commandsFrom[T: Time](opsByChip: Map[ChipSelect, SortedMap[T, Operation]]): SortedMap[T, (ChipSelect, Seq[Command])] = (for {
    (chip, opsByTime) <- opsByChip
  } yield opsByTime.collect { case (time, c: CommandMode) => time -> (chip, c.commands) }).reduce(_ ++ _)

}
