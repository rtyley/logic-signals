package com.madgag.logic

import cats.arrow.Arrow
import cats._
import cats.data._
import cats.syntax.all._

/**
 * https://eed3si9n.com/herding-cats/Isomorphism.html#Isomorphism
 * https://github.com/typelevel/cats/issues/2
 */
object Isomorphisms {
  trait Isomorphism[Arrow[_, _], A, B] { self =>
    def to: Arrow[A, B]
    def from: Arrow[B, A]
  }
  type IsoSet[A, B] = Isomorphism[Function1, A, B]
  type <=>[A, B] = IsoSet[A, B]
}