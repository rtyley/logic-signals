package com.madgag.logic.protocol.holtek.ht1632c.signals

import com.madgag.logic.BitEndian
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MixedBits {

  object Parser {
    def unit[T](x: T): Parser[T] = Parser { input => Some((x, input)) }

    def opt[T](optX: Option[T]): Parser[T] = Parser { input => optX.map((_, input)) }

    def extract(numBits: Int, bitEndian: BitEndian, rw: ReadOrWrite): Parser[BitVector] = Parser { input =>
      val chunk = input.take(numBits)
      Option.when(chunk.size == numBits && chunk.forall(_.rw == rw))((bitEndian.convert(chunk.map(_.value)), input.drop(numBits)))
    }

    def rep[T](using itemParser: Parser[T]): Parser[Seq[T]] = Parser { input =>
      val elems = new ListBuffer[T]

      @tailrec def applyp(in0: Seq[RWBit]): ParseResult[List[T]] =
        if (in0.isEmpty) Some((elems.toList, in0))
        else itemParser.parse(in0) match {
          case None => None
          case Some((x, rest)) => elems += x; applyp(rest)
        }

      applyp(input)
    }
  }

  case class Parser[+A](parse: Seq[RWBit] => ParseResult[A]) {
    def accepts(l: Seq[RWBit]): Boolean = parse(l).exists(_._2.isEmpty)

    def flatMap[B](f: A => Parser[B]): Parser[B] = Parser {
      parse(_).flatMap { case (x, midput) => f(x).parse(midput) }
    }

    def map[B](f: A => B): Parser[B] = Parser {
      parse(_).map { case (x, midput) => (f(x), midput) }
    }

    def orElse[X](that: Parser[X]): Parser[Either[X, A]] = Parser { input =>
      parse(input).map {
        case (a, rest) => (Right(a), rest)
      }.orElse(that.parse(input).map {
        case (x, rest) => (Left(x), rest)
      })
    }
  }

  type ParseResult[+T] = Option[(T, Seq[RWBit])]

}
