package com.yannmoisan.aoc

object Day9 extends Puzzle {

  override def part1 = l => parseA(l.head).length

  override def part2 = l => parseB(l.head)

  val regex = """(?:\((\d+)x(\d+)\))?(.*)""".r

  case class State(produced: String, remains: String)

  def parseA(s: String): String = {
    val mat = regex.findFirstMatchIn(s)
    mat match {
      case Some(m) =>
        (Option(m.group(1)), Option(m.group(2)), m.group(3)) match {
          case (Some(nbChar), Some(nbRepeat), s) =>
            val (prefix, suffix) = s.splitAt(nbChar.toInt)
            prefix * (nbRepeat.toInt) + parseA(suffix)
          case (None, None, s) =>
            val (prefix, suffix) = s.splitAt(s.indexOf('('))
            if (prefix.isEmpty) suffix else prefix + parseA(suffix)
        }
    }
  }

  def parseB(s: String): Long = {
    val mat = regex.findFirstMatchIn(s)
    mat match {
      case Some(m) =>
        (Option(m.group(1)), Option(m.group(2)), m.group(3)) match {
          case (Some(nbChar), Some(nbRepeat), s) =>
            val (prefix, suffix) = s.splitAt(nbChar.toInt)
            //parseB(prefix * (nbRepeat.toInt) + suffix)
            nbRepeat.toInt * parseB(prefix) + parseB(suffix)
          case (None, None, s) =>
            val (prefix, suffix) = s.splitAt(s.indexOf('('))
            if (prefix.isEmpty) suffix.length else prefix.length + parseB(suffix)
        }
    }
  }

}
