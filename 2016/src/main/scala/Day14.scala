// http://stackoverflow.com/questions/5992778/computing-the-md5-hash-of-a-string-in-scala
// http://stackoverflow.com/questions/38855843/scala-one-liner-to-generate-md5-hash-from-string
object Day14 extends SinglePuzzle[Int, Int] {

  def md5x2016(s: String): String =
    (1 to 2017).foldLeft(s) { case (acc, _) => MD5.md5(acc) }

  def contains3dups(e: (String, Int)) =
    e._1
      .sliding(3).toList
      .find(s => s(0) == s(1) && s(1) == s(2))
      .map(s => (s.head, e._2))

  def contains5(stream: LazyList[(String, Int)], index: Int, c: Char) =
    !stream.drop(index).take(1000).filter(_._1.contains(c.toString * 5)).isEmpty

  def part(f: String => String) = { (_: String) =>
    val s: LazyList[(String, Int)] = LazyList
      .from(0)
      .map(i => f(s"cuanljph$i"))
      .zipWithIndex

    s.flatMap(contains3dups)
      .filter(e => contains5(s, e._2 + 1, e._1))
      .take(64)
      .last
      ._2
  }

  override def part1(s: String): Int = part(MD5.md5)(s)

  override def part2(s: String): Int = part(md5x2016)(s)
}
