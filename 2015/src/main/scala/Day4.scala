import java.security.MessageDigest

object Day4 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    val md = MessageDigest.getInstance("MD5")
    Iterator
      .from(0).map(i =>
        md.digest(s"$input$i".getBytes)
          .map(b => f"$b%02x")
          .mkString
      ).indexWhere(_.startsWith("00000"))
  }

  override def part2(input: String): Int = {
    val md = MessageDigest.getInstance("MD5")
    Iterator
      .from(0).map(i =>
        md.digest(s"$input$i".getBytes)
          .map(b => f"$b%02x")
          .mkString
      ).indexWhere(_.startsWith("000000"))
  }
}
