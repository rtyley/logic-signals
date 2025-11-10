package com.madgag.logic

import cats.kernel.Order
import spire.algebra.Order
import spire.math.*
import spire.math.interval.{Open, ValueBound}

import scala.language.implicitConversions

type BoundedInterval[A] = Bounded[A] | Point[A]

object BoundedInterval {
  def closed[A: Order](lower: A, upper: A): BoundedInterval[A] =
    Interval.closed(lower, upper).asInstanceOf[BoundedInterval[A]] // NASTY

  def open[A: Order](lower: A, upper: A): BoundedInterval[A] =
    Interval.open(lower, upper).asInstanceOf[BoundedInterval[A]] // NASTY

  def openUpper[A: Order](lower: A, upper: A): BoundedInterval[A] =
    Interval.openUpper(lower, upper).asInstanceOf[BoundedInterval[A]] // NASTY

  def openLower[A: Order](lower: A, upper: A): BoundedInterval[A] =
    Interval.openLower(lower, upper).asInstanceOf[BoundedInterval[A]] // NASTY

  def point[A: Order](p: A): BoundedInterval[A] =
    Interval.point(p).asInstanceOf[BoundedInterval[A]] // NASTY

  extension [A: Order](lhs: BoundedInterval[A])
    def boundedUnion(rhs: BoundedInterval[A]): BoundedInterval[A] =
      lhs.union(rhs).asInstanceOf[BoundedInterval[A]] // NASTY

    def boundedIntersect(rhs: Interval[A]): BoundedInterval[A] | Empty[A] =
      lhs.intersect(rhs).asInstanceOf[BoundedInterval[A] | Empty[A]] // NASTY

    def openingLowerBound: Interval[A] =
      Interval.fromBounds(Open(lhs.lowerValueBound.a), lhs.upperValueBound)

    def opened: Interval[A] = Interval.open(
      lhs.lowerValueBound.a,
      lhs.upperValueBound.a,
    )

  extension [A](ns: BoundedInterval[A] | Above[A])
    def lowerValueBound: ValueBound[A] = ns.lowerBound match { // ideally, the compiler would be able to determine without match!
      case l: ValueBound[A] => l
      case _ => throw IllegalStateException() // Bounded, Point & Above all have lowerBound of type ValueBound
    }

  extension [A](ns: BoundedInterval[A] | Below[A])
    def upperValueBound: ValueBound[A] = ns.upperBound match { // ideally, the compiler would be able to determine without match!
      case u: ValueBound[A] => u
      case _ => throw IllegalStateException() // Bounded, Point & Below all have upperBound of type ValueBound
    }
}
