package com.madgag.logic

import com.madgag.logic.time.Time
import com.madgag.logic.time.Time.*
import com.madgag.scala.collection.decorators.*
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers.shouldBe
import spire.math.Interval
import spire.math.interval.ValueBound

import java.time.Duration
import java.time.Duration.{ofDays, ofMillis}

object TestKit {
  case class CharDuration(duration: Duration)

  val High: Set[Char] = Set('1', '█')

  def displayCharFor(boolean: Boolean): Char = if (boolean) '█' else '▁'

  def samplesFor(timeline: String): Seq[Event[Delta, Boolean]] = for {
    (x, timeMs) <- (timeline + timeline.last).zipWithIndex if x != '.'
  } yield Event(ofMillis(timeMs), High(x))

  /** Guarantees that unification occurs and consecutive intervals alternate state.
   */
  def intervalsFor(timeline: String)(using cd: CharDuration): Seq[(BoundedInterval[Delta], Boolean)] = {
    val punk = timeline.map(High).foldLeft(List.empty[(Int, Boolean)]) { (acc, bool) =>
      acc.headOption.fold(List(1 -> bool)) { (lastCount, lastBool) =>
        if (lastBool == bool) (lastCount + 1, bool) +: acc.tail
        else (1, bool) +: acc
      }
    }
    punk.reverse.foldLeft(0 -> Seq.empty[(BoundedInterval[Delta], Boolean)]) { case ((accIndex, accSeq), (count, bool)) =>
      val end = accIndex + count
      end -> (accSeq :+ (BoundedInterval.openUpper(
        cd.duration.multipliedBy(accIndex),
        cd.duration.multipliedBy(end)) -> bool))
    }._2
  }

  def signalFor(timeline: String)(using cd: CharDuration): Signal[Delta] = {
    val intervals = intervalsFor(timeline)

    val sig = Signal(intervals)

    sig.interval.duration shouldBe cd.duration.multipliedBy(timeline.length)
    sig
  }

  def signals[A](sigs: (A, String)*)(using CharDuration) = ChannelSignals[Delta, A](sigs.toMap.mapV(signalFor))



  def containedBy[T : Time : Choose](interval: Interval[T]): Gen[T] = {
    require(interval.nonEmpty) // Spire Gen instance can create Empty intervals, thankfully https://github.com/typelevel/spire/blob/0fe5a6a9714181a20fc9cef4c8b2af088ff2b4c9/laws/src/main/scala/spire/laws/gen.scala#L177-L184
    val bigTime = ofDays(1)
    ((interval.lowerBound, interval.upperBound) match {
      case (l: ValueBound[T], u: ValueBound[T]) =>
        Gen.frequency[T](
          (4 -> Gen.choose(l.a, u.a)) +: interval.closedBounds.toSeq.map(1 -> Gen.const(_))*
        )
      case (l: ValueBound[T], _) => Gen.choose(l.a, l.a.add(bigTime))
      case (_, u: ValueBound[T]) => Gen.choose(u.a.add(bigTime.negated()), u.a)
      case _ =>
        val zero = summon[Time[T]].Zero
        Gen.choose(zero.add(bigTime.negated()), zero.add(bigTime))
    }).suchThat(x => interval.contains(x))
  }

  def arbFlipTimesSignal(arbDelta: Arbitrary[Delta] = Arbitrary(Choose.chooseJavaDuration.choose(Duration.ZERO, Duration.ofNanos(100)))): Arbitrary[FlipTimesSignal[Delta]] = {
    given Arbitrary[Delta] = arbDelta
    Arbitrary(for {
      interval <- spire.laws.gen.boundedInterval[Delta].suchThat(_.nonEmpty).map(_.asInstanceOf[BoundedInterval[Delta]])
      initialState <- Arbitrary.arbitrary[Boolean]
      // intervalForValidFlipTimes = interval.openingLowerBound // we disallow flips at the lower bound
      flipTimes <-
        if (interval.isEmpty) Gen.const(List.empty)
        else Gen.listOf(containedBy(interval))
    } yield FlipTimesSignal(interval, initialState, flipTimes.toIndexedSeq.sorted.distinct))
  }

  def arbSignal(arbDelta: Arbitrary[Delta] = Arbitrary(Choose.chooseJavaDuration.choose(Duration.ZERO, Duration.ofNanos(100)))): Arbitrary[Signal[Delta]] = {
    val arbFTS = arbFlipTimesSignal(arbDelta)
    Arbitrary(arbFTS.arbitrary.map(identity))
  }
}
