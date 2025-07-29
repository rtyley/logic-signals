package com.madgag.logic

case class GpioPin(number: Int) extends AnyVal {
  override def toString: String = s"GP$number"
}
