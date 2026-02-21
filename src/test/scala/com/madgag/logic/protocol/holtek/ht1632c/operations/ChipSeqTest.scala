package com.madgag.logic.protocol.holtek.ht1632c.operations

import com.madgag.logic.BoundedInterval
import com.madgag.logic.protocol.holtek.ht1632c.Channel.ChipSelect.{Follower, Leader}
import com.madgag.logic.time.Time.*
import com.madgag.logic.time.Timed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Duration.ofSeconds

class ChipSeqTest extends AnyFlatSpec with should.Matchers {
  "ChipSeq" should "be able to drop time" in {
    val chipSeq: ChipSeq[Timed[Delta, String]] = Seq(
      ChipVal(Leader, Timed(BoundedInterval.closed(ofSeconds(1),ofSeconds(2)), "Foo")),
      ChipVal(Follower.One, Timed(BoundedInterval.closed(ofSeconds(4),ofSeconds(6)), "Bar"))
    )

    chipSeq.dropTime shouldBe Seq(
      ChipVal(Leader, "Foo"),
      ChipVal(Follower.One, "Bar")
    )
  }
}
