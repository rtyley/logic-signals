package com.madgag.logic

import com.madgag.logic.SliceBound.{LowerInclusive, UpperExclusive}
import scodec.bits.BitVector
import spire.math.Interval
import spire.math.interval.{EmptyBound, Unbound, ValueBound}

import java.util
import scala.collection.Searching.*

extension [A](ns: Interval[A])
  def valueBounds: Set[A] = Set(ns.lowerBound, ns.upperBound).collect {
    case vb: ValueBound[A] => vb.a
  }

enum SliceBound(isInclusive: Boolean):
  def stepFor(closedValueBound: Boolean): Int = if (closedValueBound != isInclusive) 1 else 0
  
  case LowerInclusive extends SliceBound(true) // Found, open, inclusive implies must increment
  case UpperExclusive extends SliceBound(false) // Found, exclusive, closed implies must increment

extension [T: Ordering](ns: IndexedSeq[T])
  def subInterval(i: Interval[T]): IndexedSeq[T] = {
    def where(x: ValueBound[T], sliceBound: SliceBound): Int = ns.search(x.a) match {
      case Found(foundIndex) => foundIndex + sliceBound.stepFor(x.isClosed)
      case InsertionPoint(insertionPoint) => insertionPoint
    }
    
    (i.lowerBound, i.upperBound) match {
      case (l: ValueBound[T], u: ValueBound[T]) => ns.slice(where(l, LowerInclusive), where(u, UpperExclusive))
      case (l: ValueBound[T], Unbound()) => ns.slice(where(l, LowerInclusive), ns.size)
      case (Unbound(), u: ValueBound[T]) => ns.slice(0, where(u, UpperExclusive))
      case (Unbound(), Unbound()) => ns
      case (EmptyBound(), _) | (_, EmptyBound()) => IndexedSeq.empty
    }
  }
