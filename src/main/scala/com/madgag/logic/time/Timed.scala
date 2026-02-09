package com.madgag.logic.time

import cats.*
import com.madgag.logic.BoundedInterval

case class Timed[T: Time, A](interval: BoundedInterval[T], value: A)

type TimedF[T] = [A] =>> Timed[T, A]

def dropTime[T: Time] = new(TimedF[T] ~> cats.Id):
  def apply[A](t: Timed[T, A]): A = t.value

given [T: Time]: Functor[TimedF[T]] with
  def map[A, B](fa: Timed[T, A])(f: A => B): Timed[T, B] = fa.copy(value = f(fa.value))