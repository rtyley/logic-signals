package com.madgag.logic.protocol.holtek.ht1632c.operations

import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits.Parser
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.{Read, Write}
import com.madgag.logic.BitEndian.{BigFirst, LittleFirst}
import com.madgag.logic.protocol.holtek.ht1632c.LedAddress
import com.madgag.logic.protocol.holtek.ht1632c.operations.DataOperation.MemoryAddress
import com.madgag.logic.protocol.holtek.ht1632c.signals.{MixedBits, ReadOrWrite}
import scodec.*
import scodec.bits.*

import scala.collection.mutable.ListBuffer
import com.madgag.scala.collection.decorators.*

import scala.annotation.tailrec

sealed trait Operation

object Operation {
  given MixedBits.Parser[Operation] = summon[Parser[OpType]].flatMap(_.parser)
}

sealed trait DataOperation extends Operation {
  val address: MemoryAddress
}

object DataOperation {

  case class MemoryAddress(value: Byte) {
    override val toString: String = s"@${value.toInt.toHexString}"
  }
  object MemoryAddress {
    given MixedBits.Parser[MemoryAddress] =
      Parser.extract(7, BigFirst, Write).map(bv => MemoryAddress(bv.padLeft(8).toByte()))
  }

  case class Data(value: BitVector) {
    override val toString: String = value.toIndexedSeq.map(b => if (b) "💡" else "⚫").mkString
  }

  object Data {
    def parse(rw: ReadOrWrite): Parser[Data] = Parser.extract(4, LittleFirst, rw).map(Data(_))
  }

  case class Access(readOrWrite: ReadOrWrite, data: Data) {
    override val toString: String = {
      val str = data.toString
      readOrWrite match {
        case Read => s"Read:$str"
        case Write => str
      }
    }
  }

  object Access {
    def parse(rw: ReadOrWrite): Parser[Access] = Data.parse(rw).map(Access(rw, _))

    given MixedBits.Parser[Access] = parse(Read).orElse(parse(Write)).map(_.merge)
  }

  case class ReadMode(address: MemoryAddress, values: Seq[Data]) extends DataOperation

  case object ReadMode {
    given MixedBits.Parser[ReadMode] = for {
      memoryAddress <- summon[Parser[MemoryAddress]]
      values <- Parser.rep[Data](using Data.parse(Read))
    } yield ReadMode(memoryAddress, values)
  }

  case class WriteMode(address: MemoryAddress, values: Seq[Access]) extends DataOperation {

    val successive: Map[ReadOrWrite, Seq[Option[Data]]] = {
      val boo = ReadOrWrite.values.map(_ -> new ListBuffer[Option[Data]]).toMap

      for (access <- values) {
        val rw = access.readOrWrite
        boo(rw).append(Some(access.data))

        rw match {
          case Write =>
            boo(Read).padTo(boo(Write).size, None) // We're not going to read from this mem location, we've just written to it
          case Read =>
            boo(Write).padTo(boo(Read).size - 1, None) // We're not going to write to the previous location, that's for sure
        }
      }
      boo.mapV(_.toSeq)
    }

    val grouped: Map[ReadOrWrite, Map[Int, Seq[Data]]] = successive.mapV { voom =>
      @tailrec def grump(remain: Seq[(Option[Data], Int)], acc: Seq[(Int, Seq[Data])]): Seq[(Int, Seq[Data])] = {
        val stuff = remain.dropWhile(_._1.isEmpty)
        if (stuff.isEmpty) acc else {
          val populatedChunk = stuff.takeWhile(_._1.isDefined)
          val index = populatedChunk.head._2
          val cleanedChunk: Seq[Data] = populatedChunk.flatMap(_._1)
          grump(stuff.drop(populatedChunk.size), acc :+ (index, cleanedChunk))
        }
      }

      grump(voom.zipWithIndex, Seq.empty).toMap
    }

    lazy val writesByLedAddress: Map[LedAddress, Boolean] = (for {
      (dataOpt, offset) <- successive(Write).zipWithIndex
      data <- dataOpt.toSeq
      dataIndex <- 0 to 3
    } yield LedAddress(address.value + offset, dataIndex) -> data.value(dataIndex)).toMap

    override val toString: String = s"Write $address (${values.size}*4 bits)\n${values.grouped(2).map(_.mkString).mkString("\n")}"
  }
  
  case object WriteMode {
    given MixedBits.Parser[WriteMode] = for {
      memoryAddress <- summon[Parser[MemoryAddress]]
      values <- Parser.rep[Access]
    } yield WriteMode(memoryAddress, values)
  }
}

case class Command(value: BitVector) {
  override val toString: String = {
    val b = value.toBin
    s"${b.take(4)}-${b.substring(4,8)}-X"
  }
}

object Command {
  given MixedBits.Parser[Command] = Parser.extract(9, BigFirst, Write).map(Command(_))

  trait Code {
    def matches(bitVector: BitVector): Boolean
  }

  case class SimpleCode(pattern: String) extends Code {
    val barePattern = pattern.replace("-", "")
    val mask = BitVector.bits(barePattern.map(_ == 'X'))
    val bits = BitVector.bits(barePattern.map(_ == '1'))

    override def matches(bitVector: BitVector): Boolean = (bitVector & mask) == bits
  }

  val SlaveMode = SimpleCode("0001-0XXX-X")
}

case class CommandMode(commands: Seq[Command]) extends Operation
object CommandMode {
  given MixedBits.Parser[CommandMode] = Parser.rep[Command].map(CommandMode(_))
}

