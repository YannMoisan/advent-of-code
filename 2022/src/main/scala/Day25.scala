object Day25 extends MultiPuzzle[String, Int] {
  override def part1(input: Iterator[String]): String =
    intToSnafu(input.map(snafuToInt).sum)

  // 2=-01
  def snafuToInt(s: String): Long = {
    var sum = 0L
    s.indices.foreach { i =>
      sum += (s(s.length - 1 - i) match {
        case '2' => 2
        case '1' => 1
        case '0' => 0
        case '-' => -1
        case '=' => -2
      }) * math.pow(5, i.toDouble).toLong
    }
    sum
  }

  // 2=-01
  def intToSnafu(v: Long): String = {
    println(v)
    var cur = v
    var i   = 0
    val sb  = new StringBuilder()
    while (cur != 0) {
      val remainder = (5 + cur) % 5
      sb.append(remainder match {
        case 0 => '0'
        case 1 => '1'
        case 2 => '2'
        case 3 => '='
        case 4 => '-'
      })
      cur -= (remainder match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => -2
        case 4 => -1
      })
      cur = cur / 5
      i += 1
    }
    sb.reverse.toString
  }

  override def part2(input: Iterator[String]): Int = 42
}
