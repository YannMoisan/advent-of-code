import scala.collection.mutable

object Day14 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    var mask = ""
    val mem  = mutable.Map[String, Long]()

    input.foreach {
      case s"mask = $mask_" => mask = mask_
      case s"mem[$adr] = $value" =>
        val str3 = mask
          .zip(value.toLong.toBinaryString.reverse.padTo(36, '0').reverse).map {
            case ('X', v) => v
            case (ch, _)  => ch
          }.mkString

        mem(adr) = java.lang.Long.parseLong(str3, 2)
    }
    mem.map(_._2).sum
  }

  override def part2(input: Iterator[String]): Long = {
    var mask = ""
    val mem  = mutable.Map[Long, Long]()

    input.foreach {
      case s"mask = $mask_" => mask = mask_
      case s"mem[$adr] = $value" =>
        unfold(apply(mask, adr)).foreach(adr2 =>
          mem(java.lang.Long.parseLong(adr2, 2)) = value.toLong
        )
    }
    mem.map(_._2).sum
  }

  def unfold(s: String): Seq[String] = {
    val idx = s.indexOf('X')
    if (idx != -1) {
      Seq(unfold(StringUtils.update(s, idx, '0')), unfold(StringUtils.update(s, idx, '1'))).flatten
    } else Seq(s)
  }

  def apply(mask: String, value: String): String =
    mask
      .zip(value.toLong.toBinaryString.reverse.padTo(36, '0').reverse).map {
        case ('0', v) => v
        case (ch, _)  => ch
      }.mkString
}

object StringUtils {
  def update(s: String, index: Int, newChar: Char): String = {
    val arr = s.toCharArray
    arr(index) = newChar
    arr.mkString
  }
}
