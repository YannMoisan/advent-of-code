object Day10 extends SinglePuzzle[Int, Int] {
  // conclusion 1 : ArrayBuffer > ListBuffer

  // [day=10] (t=351018ms) 492982
  //               1709308ms
  override def part1(input: String): Int = {
    val input = "1321131112"
    Iterator.iterate(input)(lookAndSay).drop(40).next().length
  }

  override def part2(input: String): Int = {
    val input = "1321131112"
    Iterator.iterate(input)(lookAndSay).drop(50).next().length
  }

  def lookAndSay(s: String): String = {
    val sb           = new StringBuilder()
    var lastCh: Char = s(0)
    var count: Int   = 1
    s.substring(1).foreach { ch =>
      if (lastCh == ch) {
        count += 1
      } else {
        sb.append((count + 48).toChar)
        sb.append(lastCh)
        lastCh = ch
        count = 1
      }
    }
    sb.append((count + 48).toChar)
    sb.append(lastCh)

    sb.toString()
  }
}
