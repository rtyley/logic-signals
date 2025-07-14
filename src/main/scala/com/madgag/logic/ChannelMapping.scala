package com.madgag.logic

import com.madgag.scala.collection.decorators.*

case class ChannelMapping[C](fieldsInPreferredOrder: (String, C)*) {
  val m: Map[String, C] = fieldsInPreferredOrder.toMap
  private val reverseM: Map[C, String] = m.map(_.swap)
  
  val channels: Set[C] = m.values.toSet
  
  def csvFieldsFor(on: Set[C]): Map[String, String] = channels.map {
    channel => reverseM(channel) -> (if (on.contains(channel)) "1" else "0")
  }.toMap

  def parseState(r: Map[String, String]): Set[C] = for {
    (col, boolText) <- r.view.filterKeys(m.keySet).toSet if boolText.toInt != 0
    c <- m.get(col)
  } yield c

  def parse[T: Time](r: Map[String, String], timeParser: TimeParser[T]): Event[T, Set[C]] =
    Event(timeParser.timeFormat.to(r(timeParser.fieldName)), parseState(r))

  def groupByChannel[T: Time](d: Iterable[Event[T, Set[C]]]): Map[C, Iterable[Event[T, Boolean]]] = (for {
    channel <- channels
  } yield channel -> d.map(_.map(_.contains(channel)))).toMap

  def signals[T: Time](d: Iterable[Event[T, Set[C]]]): ChannelSignals[T, C] = 
    ChannelSignals(groupByChannel(d).mapV(foo => Signal(foo)))
  
}
