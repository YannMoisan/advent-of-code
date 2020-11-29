import java.security.MessageDigest

object Day4 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    val md    = MessageDigest.getInstance("MD5")
    var i     = 0
    var found = false
    while (!found) {
      val s = md
        .digest(s"$input$i".getBytes)
        .map(b => f"$b%02x")
        .mkString
      if (s.startsWith("00000")) found = true else i += 1
    }
    i
  }

  override def part2(input: String): Int = {
    val md    = MessageDigest.getInstance("MD5")
    var i     = 0
    var found = false
    while (!found) {
      val s = md
        .digest(s"$input$i".getBytes)
        .map(b => f"$b%02x")
        .mkString
      if (s.startsWith("000000")) found = true else i += 1
    }
    i
  }
}
