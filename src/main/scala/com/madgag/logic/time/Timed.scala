package com.madgag.logic.time

import cats.syntax.all.*
import cats.*
import com.madgag.logic.BoundedInterval

case class Timed[T: Time, A](interval: BoundedInterval[T], value: A)

type TimedF[T] = [A] =>> Timed[T, A]

object TimedF {
  def dropTime[T: Time]: TimedF[T] ~> Id = new(TimedF[T] ~> cats.Id):
    def apply[A](t: Timed[T, A]): A = t.value

  given [T: Time]: Functor[TimedF[T]] with
    def map[A, B](fa: Timed[T, A])(f: A => B): Timed[T, B] = fa.copy(value = f(fa.value))

  given timedTraverse[T](using Time[T]): Traverse[[A] =>> Timed[T, A]] with

    override def traverse[G[_] : Applicative, A, B](fa: Timed[T, A])(f: A => G[B]): G[Timed[T, B]] =
      f(fa.value).map { b =>Timed(fa.interval, b) }

    override def foldLeft[A, B](fa: Timed[T, A], b: B)(f: (B, A) => B): B = f(b, fa.value)

    override def foldRight[A, B](fa: Timed[T, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)
}

