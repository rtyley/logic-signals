package com.madgag.logic

import cats.kernel.Order
import spire.algebra.{AdditiveMonoid, Order}
import spire.math.*
import spire.math.interval.ValueBound

import java.time.{Duration, Instant}
import scala.language.implicitConversions

trait Time[T](using val ev: Ordering[T]) {
  val toOrder: Order[T] = Order.fromOrdering[T](using ev)

  val Zero: T

  def between(start: T, end: T): Duration
  
  def add(time: T, duration: Duration): T
}

trait TimeToOrderingConversion {
  // given [T: Ordering]: Conversion[Time[T], Order[T]] = _.toOrder // lookups fail :(

  implicit def orderForTime[A](using ev: Time[A]): Order[A] = ev.toOrder

//   implicit def orderingForTime[A](using ev: Time[A]): Ordering[A] = ev.ev
}

object Time extends TimeToOrderingConversion {
  type Delta = Duration

  extension [T: Time](t: T)
    def add(duration: Duration): T = summon[Time[T]].add(t, duration)

  extension [T : Time](interval: BoundedInterval[T])
    def duration: Duration = interval match {
      case b: Bounded[T] => summon[Time[T]].between(b.lower, b.upper)
      case p: Point[T] => Duration.ZERO
    }

    def lazyList(step: Duration, direction: Direction): LazyList[T] =
      val start: ValueBound[T] = direction.initialBound(interval)
      val directedStep = direction[Duration](_.negated)(step)
      LazyList.from(if start.isClosed then 0 else 1)
        .map(mult => start.a.add(directedStep.multipliedBy(mult))).takeWhile(interval.contains)

  def between[T](start: T, end: T)(using t: Time[T]): Duration = t.between(start, end)

  given AdditiveMonoid[Delta] = new AdditiveMonoid[Delta] {
    def zero: Delta = Duration.ZERO
    def plus(x: Delta, y: Delta): Delta = x.plus(y)
  }
  
  given Time[Delta] = new Time[Delta](using Ordering.ordered):
    override val Zero: Delta = Duration.ZERO
    override def between(start: Delta, end: Delta): Duration = end.minus(start)
    override def add(time: Delta, duration: Duration): Delta = time.plus(duration)

  given Time[Instant] = new Time[Instant](using Ordering.ordered):
    override val Zero: Instant = Instant.EPOCH
    override def between(start: Instant, end: Instant): Duration = Duration.between(start, end)
    override def add(time: Instant, duration: Duration): Instant = time.plus(duration)
}