// http://stackoverflow.com/questions/5992778/computing-the-md5-hash-of-a-string-in-scala
// http://stackoverflow.com/questions/38855843/scala-one-liner-to-generate-md5-hash-from-string
object Day14 extends SinglePuzzle[Int, Int] {

  def md5x2016(s: String): String =
    Iterator.iterate(s)(MD5.md5).drop(2017).next()

  def contains3dups(e: String): Option[Char] =
    e.sliding(3)
      .find(s => s(0) == s(1) && s(1) == s(2))
      .map(s => s.head)

  def contains5(stream: LazyList[String], c: Char): Boolean =
    stream.take(1000).exists(_.contains(c.toString * 5))

  def keyIndices(s: LazyList[(String, Int)]): LazyList[Int] =
    s match {
      case (str, i) #:: rest =>
        contains3dups(str) match {
          case Some(c) if contains5(rest.map(_._1), c) => i #:: keyIndices(rest)
          case _                                       => keyIndices(rest)
        }
    }

  def part(f: String => String) = { (_: String) =>
    val s: LazyList[(String, Int)] = LazyList
      .from(0)
      .map(i => f(s"cuanljph$i"))
      .zipWithIndex

    keyIndices(s)
      .take(64)
      .last
  }

  override def part1(s: String): Int = part(MD5.md5)(s)

  override def part2(s: String): Int = part(md5x2016)(s)
}
