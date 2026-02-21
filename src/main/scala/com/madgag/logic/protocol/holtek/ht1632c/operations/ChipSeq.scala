package com.madgag.logic.protocol.holtek.ht1632c.operations

import cats.syntax.all.*
import cats.*
import com.madgag.logic.protocol.holtek.ht1632c.Channel.ChipSelect
import com.madgag.logic.time.{Time, Timed}

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

extension [F[_], A](chipSeq: ChipSeq[F[A]])
  def mapK[G[_]](fk: F ~> G)(using Functor[F]): ChipSeq[G[A]] = chipSeq.mapChipVal(fk(_))

extension [F[_]: Traverse, A](chipSeq: ChipSeq[F[A]])
  def flatTraverseChipVal[B](f: A => IterableOnce[B]): ChipSeq[F[B]] =
    chipSeq.flatMapChipVal(_.traverse(f(_).iterator.toList))
