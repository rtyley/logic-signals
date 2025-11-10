package com.madgag.logic

import cats.Endo
import cats.kernel.Order
import com.madgag.logic.BoundedInterval.*
import spire.math.interval.ValueBound

enum Direction:
  case Asc
  case Desc

  def fold[T, A](asc: T => A, desc: T => A)(x: T): A = this match
    case Asc => asc(x)
    case Desc => desc(x)

  def when[T](asc: => T, desc: => T): T = this match
    case Asc => asc
    case Desc => desc

  def apply[T](reverse: T => T): Endo[T] = fold[T, T](identity, reverse)

  def approach[T]: Endo[Seq[T]] = apply(_.reverse)

  def applyTo[T]: Endo[Order[T]] = apply(Order.reverse)

  def ordering[T]: Endo[Ordering[T]] = apply(_.reverse)
  
  def initialBound[T]: BoundedInterval[T] => ValueBound[T] =
    fold(_.lowerValueBound, _.upperValueBound)
