package com.madgag.logic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spire.math.Interval
import spire.math.Interval.{closed, open, openLower, openUpper}

class SortedIndexedSeqTest extends AnyFlatSpec with should.Matchers {
  "subInterval" should "handle interval boundaries" in {
    val sortedInts = IndexedSeq(4, 6, 8, 10, 13, 15)
    sortedInts.subInterval(closed(4, 15)) shouldBe sortedInts
    sortedInts.subInterval(openUpper(4, 15)) shouldBe IndexedSeq(4, 6, 8, 10, 13)
    sortedInts.subInterval(openLower(4, 15)) shouldBe IndexedSeq(6, 8, 10, 13, 15)
    sortedInts.subInterval(open(4, 15)) shouldBe IndexedSeq(6, 8, 10, 13)

    sortedInts.subInterval(closed(5, 14)) shouldBe IndexedSeq(6, 8, 10, 13)
    sortedInts.subInterval(openUpper(5, 14)) shouldBe IndexedSeq(6, 8, 10, 13)
    sortedInts.subInterval(openLower(5, 14)) shouldBe IndexedSeq(6, 8, 10, 13)
    sortedInts.subInterval(open(5, 14)) shouldBe IndexedSeq(6, 8, 10, 13)


    sortedInts.subInterval(closed(6, 13)) shouldBe IndexedSeq(6, 8, 10, 13)
    sortedInts.subInterval(openUpper(6, 13)) shouldBe IndexedSeq(6, 8, 10)
    sortedInts.subInterval(openLower(6, 13)) shouldBe IndexedSeq(8, 10, 13)
    sortedInts.subInterval(open(6, 13)) shouldBe IndexedSeq(8, 10)
  }
}