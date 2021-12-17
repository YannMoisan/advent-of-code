import scala.collection.mutable.ListBuffer

trait Packet
case class ValuePacket(version: Int, typeId: Int, v: Long)           extends Packet
case class OpPacket(version: Int, typeId: Int, packets: Seq[Packet]) extends Packet

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day16 extends SinglePuzzle[Int, Long] {
  // do the conversion manually to keep leading zeros
  val hexaToFourBits = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  override def part1(input: String) =
    sum(parse(toBinary(input))._1)

  override def part2(input: String) = compute(parse(toBinary(input))._1)

  def toBinary(hexa: String): String = hexa.map(hexaToFourBits).mkString
  //BigInt(hexa, 16).toString(2)

  def parse(binary: String): (Packet, String) = {
    //val version = Integer.parseInt(binary.substring(0, 3), 2)
    val typeId = Integer.parseInt(binary.substring(3, 6), 2)
    if (typeId == 4) {
      parseValue(binary)
    } else {
      parseOp(binary)
    }
  }

  def parseValue(binary: String): (ValuePacket, String) = {
    val version = Integer.parseInt(binary.substring(0, 3), 2)
    val typeId  = Integer.parseInt(binary.substring(3, 6), 2)
    val rest    = binary.substring(6).grouped(5)

    // TODO Iterator.takeUntil
    var continue = true
    var res      = ""
    while (rest.hasNext && continue) {
      val next = rest.next()
      res += next.drop(1)
      if (next.head == '0') continue = false
    }
    (ValuePacket(version, typeId, BigInt.apply(res, 2).longValue), rest.mkString)
  }

  def parseOp(binary: String): (OpPacket, String) = {
    val version      = Integer.parseInt(binary.substring(0, 3), 2)
    val typeId       = Integer.parseInt(binary.substring(3, 6), 2)
    val lengthTypeId = Integer.parseInt(binary.substring(6, 7))
    if (lengthTypeId == 0) {
      val totalLengthInBits = Integer.parseInt(binary.substring(7, 7 + 15), 2)
      var toParse           = binary.substring(7 + 15, 7 + 15 + totalLengthInBits)
      val lb                = new ListBuffer[Packet]()

      while (!toParse.isEmpty) {
        val parsed = parse(toParse)
        val _      = lb.addOne(parsed._1)
        toParse = parsed._2
      }

      val rest = binary.substring(7 + 15 + totalLengthInBits)
      (OpPacket(version, typeId, lb.toSeq), rest)
    } else {
      val numberOfSubPackets = Integer.parseInt(binary.substring(7, 7 + 11), 2)
      var toParse            = binary.substring(7 + 11)
      val lb                 = new ListBuffer[Packet]()
      (0 until numberOfSubPackets).foreach { _ =>
        val parsed = parse(toParse)
        val _      = lb.addOne(parsed._1)
        toParse = parsed._2
      }
      (OpPacket(version, typeId, lb.toSeq), toParse)
    }
  }

  def sum(packet: Packet): Int =
    packet match {
      case ValuePacket(version, _, _)    => version
      case OpPacket(version, _, packets) => version + packets.map(p => sum(p)).sum
    }

  def compute(packet: Packet): Long =
    packet match {
      case ValuePacket(_, _, value) => value
      case OpPacket(_, 0, packets)  => packets.map(p => compute(p)).sum
      case OpPacket(_, 1, packets)  => packets.map(p => compute(p)).product
      case OpPacket(_, 2, packets)  => packets.map(p => compute(p)).min
      case OpPacket(_, 3, packets)  => packets.map(p => compute(p)).max
      case OpPacket(_, 5, packets)  => if (compute(packets(0)) > compute(packets(1))) 1 else 0
      case OpPacket(_, 6, packets)  => if (compute(packets(0)) < compute(packets(1))) 1 else 0
      case OpPacket(_, 7, packets)  => if (compute(packets(0)) == compute(packets(1))) 1 else 0
    }
}
