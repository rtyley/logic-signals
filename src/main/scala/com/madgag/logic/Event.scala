package com.madgag.logic

import com.madgag.logic.time.Time

case class Event[T: Time, +V](time: T, value: V) {
  def map[S](f: V => S): Event[T, S] = Event(time, f(value))
}

object Event {
  given [T: Time]: Ordering[Event[T, _]] = summon[Time[T]].ev.on[Event[T, _]](_.time)
}