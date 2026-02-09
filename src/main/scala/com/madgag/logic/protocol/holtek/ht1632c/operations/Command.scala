package com.madgag.logic.protocol.holtek.ht1632c.operations

import com.madgag.logic.BitEndian.BigFirst
import com.madgag.logic.BoundedInterval
import com.madgag.logic.BoundedInterval.*
import com.madgag.logic.bits.Nibble
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.COM.{DisplayLayout, OpenDrain}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.PWM.Levels
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.OffOn.{Off, On}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.Setting.{OffOn, Switchable}
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.SyncRole.ClockSource
import com.madgag.logic.protocol.holtek.ht1632c.operations.Command.SyncRole.ClockSource.{ExternalClock, OnChipOscillator}
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits
import com.madgag.logic.protocol.holtek.ht1632c.signals.MixedBits.Parser
import com.madgag.logic.protocol.holtek.ht1632c.signals.ReadOrWrite.Write
import scodec.bits.BitVector

/**
 * Represents the commands that can be sent in [[CommandMode]] to the Holtek HT1632C. These are typically sent when
 * initialising the device.
 *
 * [[https://cdn-shop.adafruit.com/datasheets/ht1632cv120.pdf]] - pages 12 ('Timing Diagrams') & 21 ('Command Summary')
 * [[https://github.com/user-attachments/assets/6250a385-06e3-4e5c-9244-79d43c5ce4a7]]
 */
sealed trait Command

object Command {

  private lazy val commandCache: IndexedSeq[Command] = for {
    classifier <- Nibble.All
    content <- Nibble.All
  } yield apply(classifier, content)

  /**
   * Commands are sent as 9-bit codes of the form `CCCC-cccc-X`, where `X` is 'don't care', `CCCC` is a classifier
   * for the type of command ([[Setting]], [[PWM]], etc), and `cccc` provides content (eg the level of the duty cycle
   * for [[PWM]]).
   *
   * This method uses a lookup table to provide quick access to cached instances of [[Command]].
   */
  def apply(bitVector: BitVector): Command = {
    require(bitVector.size == 9)
    val index = bitVector.take(8).toInt(signed = false)
    commandCache(index)
  }

  def apply(classifier: Nibble, content: Nibble): Command = (for {
    (classifierPattern, cf) <- CommandFinder.All if classifierPattern.matches(classifier)
    command <- cf.commandFor(content)
  } yield command).headOption.getOrElse(UnknownCode(classifier, content))

  given MixedBits.Parser[Command] = Parser.extract(9, BigFirst, Write).map(Command(_))

  trait Code {
    def matches(bitVector: BitVector): Boolean
  }

  case class NibblePattern(fixedBits: Nibble, mask: Nibble) {
    def matches(nibble: Nibble): Boolean = (nibble.bitVector & mask.bitVector) == fixedBits.bitVector
  }

  object NibblePattern {
    def apply(pattern: String): NibblePattern = {
      require(pattern.length == 4)

      def nibbleFor(p: Char => Boolean) = Nibble(BitVector.bits(pattern.map(p)))

      NibblePattern(fixedBits = nibbleFor(_ == '1'), mask = nibbleFor(c => c == '0' || c == '1'))
    }
  }

  case class CommandPattern(classifier: NibblePattern, content: NibblePattern)

  object CommandPattern {
    def apply(text: String): CommandPattern = {
      require(text.length == 11)
      require(text(4) == '-' && text.takeRight(2) == "-X")
      CommandPattern(NibblePattern(text.take(4)), NibblePattern(text.substring(5, 9)))
    }
  }

  trait CommandFinder {
    val classifier: NibblePattern

    def commandFor(contentNibble: Nibble): Option[Command]
  }

  object CommandFinder {
    val All: Map[NibblePattern, CommandFinder] =
      Seq[CommandFinder](Setting, SyncRole, COM, PWM).map(cf => cf.classifier -> cf).toMap
  }

  object Setting extends CommandFinder {
    val classifier = NibblePattern("0000")

    enum Switchable(val content: NibblePattern):
      case SystemOscillator extends Switchable(NibblePattern("000e"))
      case LedDutyCycleGenerator extends Switchable(NibblePattern("001e"))
      case Blink extends Switchable(NibblePattern("100e"))

      def apply(offOn: OffOn) = Setting(this, offOn)

    enum OffOn:
      case Off, On

    def switchableFor(content: Nibble): Option[Switchable] = Switchable.values.find(_.content.matches(content))

    def offOnFor(content: Nibble): OffOn = if (content(0, BigFirst)) On else Off

    def commandFor(content: Nibble): Option[Command] = switchableFor(content).map(Setting(_, offOnFor(content)))
  }

  case class Setting(switchable: Switchable, offOn: OffOn) extends Command {
    override val toString: String = s"$switchable: $offOn"
  }

  /**
   * Multiple Holtek HT1632C ICs can be cascaded together - the first IC should be set to take the role of Leader,
   * with subsequent ICs being Followers that receive the Leader's SYNC signal.
   *
   * The original Holtek docs use the terms 'master' & 'slave' - I've switched those terms to
   * 'leader' and 'follower' for this project.
   *
   * https://datatracker.ietf.org/doc/html/draft-knodel-terminology-14#name-master-slave
   */
  enum SyncRole(val content: NibblePattern, val clockSource: ClockSource) extends Command:
    /**
     * A Follower receives the clock signal on their OSC pin, and the SYNC signal as input on their SYN pin.
     *
     * "Set slave mode and clock source from external clock, the system clock input from OSC pin and synchronous signal
     * input from SYN pin"
     */
    case Follower extends SyncRole(NibblePattern("0XXX"), ExternalClock)
    /**
     * A Leader that generates clock from an on-chip RC oscillator, sending it to its OSC pin, and the SYNC signal to its SYN pin.
     *
     * [[https://en.wikipedia.org/wiki/RC_oscillator]]
     *
     * "Set master mode and clock source from on-chip RC oscillator, the system clock output to OSC pin and synchronous
     * signal output to SYN pin"
     */
    case RCLeader extends SyncRole(NibblePattern("10XX"), OnChipOscillator)
    /**
     * A Leader that receives an external clock signal on its OSC pin, and sends a SYNC signal on its SYN pin.
     *
     * "Set master mode and clock source from external clock, the system clock input from OSC pin and synchronous
     * signal output to SYN pin"
     */
    case ExternalClockLeader extends SyncRole(NibblePattern("11XX"), ExternalClock)

  object SyncRole extends CommandFinder {
    val classifier = NibblePattern("0001")

    enum ClockSource:
      case OnChipOscillator, ExternalClock

    override def commandFor(content: Nibble): Option[Command] = SyncRole.values.find(_.content.matches(content))
  }

  case class COM(openDrain: OpenDrain, displayLayout: DisplayLayout) extends Command

  object COM extends CommandFinder {
    val pattern = CommandPattern("0010-abXX-X")
    override val classifier: NibblePattern = pattern.classifier

    override def commandFor(content: Nibble): Option[Command] = {
      def ordinal(contentIndex: Int): Int = if content.bitVector(contentIndex) then 1 else 0

      Some(COM(OpenDrain.fromOrdinal(ordinal(3)), DisplayLayout.fromOrdinal(ordinal(2))))
    }

    /**
     * "Selectable NMOS open drain output driver and PMOS open drain output driver for commons"
     * "common pad N-MOS open drain output or P-MOS open drain output"
     *
     * [[https://en.wikipedia.org/wiki/Open_collector#Open_drain]]
     */
    enum OpenDrain:
      case NMOS, PMOS

    /**
     * "The static display memory (RAM) is organized into 64x4 bits or 96x4 bits and is used to store the display data.
     * If 32 ROW &  8 COM is selected, the RAM size is 64x4 bits.
     * If 24 ROW & 16 COM is selected, the RAM size is 96x4 bits."
     */
    enum DisplayLayout(rows: Int, common: Int):
      case `32x8` extends DisplayLayout(rows = 32, common = 8)
      case `24x16` extends DisplayLayout(rows = 24, common = 16)

      def bits: Int = rows * common
  }

  /**
   * 16-level PWM brightness control
   *
   * "The Display Dimming capabilities of the HT1632 are very versatile. The whole display can be dimmed using pulse
   * width modulation techniques for the ROW driver with the Dimming command."
   *
   * [[https://cdn-shop.adafruit.com/datasheets/ht1632cv120.pdf]] - page 9 ('Digital Dimming')
   */
  case class PWM(duty: Int) extends Command {
    require(Levels.contains(duty))

    override val toString: String = s"PWM: $duty/${Levels.upperValueBound.a}"
  }

  object PWM extends CommandFinder {
    val Levels: BoundedInterval[Int] = BoundedInterval.closed(1, 16)

    val pattern = CommandPattern("101X-dddd-X")
    override val classifier: NibblePattern = pattern.classifier

    override def commandFor(content: Nibble): Option[Command] = Some(PWM(1 + content.bitVector.toInt(signed = false)))
  }

  case class UnknownCode(classifier: Nibble, content: Nibble) extends Command

}
