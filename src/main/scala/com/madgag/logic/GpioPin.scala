package com.madgag.logic

case class GpioPin(number: Int) extends AnyVal with Ordered[GpioPin] {
  override def toString: String = s"GP$number"

  override def compare(that: GpioPin): Int = number.compare(that.number)
}

