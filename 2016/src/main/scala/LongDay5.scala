// http://stackoverflow.com/questions/5992778/computing-the-md5-hash-of-a-string-in-scala
// http://stackoverflow.com/questions/38855843/scala-one-liner-to-generate-md5-hash-from-string
object LongDay5 extends SinglePuzzle[String, String] {

  override def part1(s: String): String =
    Stream
      .from(1)
      .map(i => MD5.md5(s"uqwqemis$i"))
      .filter(_.startsWith("00000"))
      .map(_(5))
      .take(8)
      .mkString("")

  override def part2(s: String): String = part1(s)
}
