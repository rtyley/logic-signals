package com.madgag.logic.protocol.holtek.ht1632c.operations

import cats.*
import cats.kernel.Order.*
import cats.syntax.all.*
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.ChipSelect
import com.madgag.logic.protocol.holtek.ht1632c.ChipLed
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.WriteMode
import com.madgag.logic.time.Time.orderForTime
import com.madgag.logic.time.{Time, Timed}

import java.time.Duration

case class ChipVal[A](chipSelect: ChipSelect, value: A) {
  def map[B](f: A => B): ChipVal[B] = copy(value = f(value))

  // Is this flatMap?! Probably not...
  def flatMap[B](f: A => Iterable[B]): Iterable[ChipVal[B]] = for {
    b <- f(value)
  } yield copy(value = b)
}

type ChipSeq[A] = Seq[ChipVal[A]]

extension [A](chipSeq: ChipSeq[A])

  def groupByChip: Map[ChipSelect, Seq[A]] = chipSeq.groupMap(_.chipSelect)(_.value)

  def mapChipVal[B](f: A => B): ChipSeq[B] = chipSeq.map(_.map(f))

  def flatMapChipVal[B](f: A => Iterable[B]): ChipSeq[B] = chipSeq.flatMap(_.flatMap(f))

extension [T: Time, A](chipSeq: ChipSeq[Timed[T,A]])
  def dropTime: ChipSeq[A] = chipSeq.mapChipVal(_.value)

  def splitByGaps(minGapDuration: Duration): Seq[Timed[T, ChipSeq[A]]] = chipSeq.foldLeft(List.empty[ChipSeq[Timed[T,A]]]) {
    (acc, chipVal) =>
      acc.headOption.filter(x => Time.between(x.map(_.value.interval.upperValueBound.a).max, chipVal.value.interval.lowerValueBound.a) < minGapDuration)
        .fold(Seq(chipVal) :: acc) { sameChunk => (sameChunk :+ chipVal) :: acc.tail }
  }.reverse.map { (entry: ChipSeq[Timed[T,A]]) => Timed(entry.map(_.value.interval).reduce(_ boundedUnion _), entry.dropTime) }

extension (chipSeq: ChipSeq[WriteMode])
  def resultingChipLedState: Map[ChipLed, Boolean] = chipSeq.foldLeft(Map.empty) {
    (acc, chipVal) => acc ++ chipVal.value.writesByLedAddress.map((la, state) => ChipLed(chipVal.chipSelect, la) -> state)
  }

extension [F[_], A](chipSeq: ChipSeq[F[A]])
  def mapK[G[_]](fk: F ~> G)(using Functor[F]): ChipSeq[G[A]] = chipSeq.mapChipVal(fk(_))

extension [F[_]: Traverse, A](chipSeq: ChipSeq[F[A]])
  def flatTraverseChipVal[B](f: A => IterableOnce[B]): ChipSeq[F[B]] =
    chipSeq.flatMapChipVal(_.traverse(f(_).iterator.toList))
