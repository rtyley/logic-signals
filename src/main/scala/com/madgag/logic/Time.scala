package com.madgag.logic

import cats.kernel.Order
import spire.algebra.AdditiveMonoid
import spire.math.interval.ValueBound
import spire.math.{Bounded, Interval}

import java.time.temporal.Temporal
import java.time.{Duration, Instant}
import scala.language.implicitConversions

trait Time[T](using ev: Ordering[T]) {
  val toOrder: Order[T] = Order.fromOrdering[T](using ev)

  def between(start: T, end: T): Duration
  
  def add(time: T, duration: Duration): T
}

trait TimeToOrderingConversion {
  // given [T: Ordering]: Conversion[Time[T], Order[T]] = _.toOrder // lookups fail :(

  implicit def orderForTime[A](using ev: Time[A]): Order[A] = ev.toOrder
}

object Time extends TimeToOrderingConversion {
  type Delta = Duration
  
  extension [T: Time](ns: Interval[T])
    def duration: Duration = ns match {
      case b: Bounded[T] => summon[Time[T]].between(b.lower, b.upper)
      case _ => ???
    }
    
    def lazyList(step: Duration): LazyList[T] = ns match {
      case b: Bounded[T] => {
        LazyList.from(0).map(mult => summon[Time[T]].add(b.lower, step.multipliedBy(mult))).takeWhile(ns.contains)
      }
      case _ => ???
    }

  def between[T](start: T, end: T)(using t: Time[T]): Duration = t.between(start, end)

  given AdditiveMonoid[Delta] = new AdditiveMonoid[Delta] {
    def zero: Delta = Duration.ZERO
    def plus(x: Delta, y: Delta): Delta = x.plus(y)
  }
  
  given Time[Delta] = new Time[Delta](using Ordering.ordered):
    override def between(start: Delta, end: Delta): Duration = end.minus(start)
    override def add(time: Delta, duration: Duration): Delta = time.plus(duration)

  given Time[Instant] = new Time[Instant](using Ordering.ordered):
    override def between(start: Instant, end: Instant): Duration = Duration.between(start, end)
    override def add(time: Instant, duration: Duration): Instant = time.plus(duration)
}