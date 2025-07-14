package com.madgag.logic

case class Event[T: Time, V](time: T, value: V) {
  def map[S](f: V => S): Event[T, S] = Event(time, f(value))
}
