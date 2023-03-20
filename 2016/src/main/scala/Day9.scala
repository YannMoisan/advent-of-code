object Day9 extends SinglePuzzle[Int, Long] {

  override def part1(s: String): Int = parseA(s).length

  override def part2(s: String): Long = parseB(s)

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
          case _ => throw new IllegalStateException()
        }
      case _ => throw new IllegalStateException()
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
            if (prefix.isEmpty) suffix.length.toLong else prefix.length + parseB(suffix)
          case _ => throw new IllegalStateException()
        }
      case _ => throw new IllegalStateException()
    }
  }

}
