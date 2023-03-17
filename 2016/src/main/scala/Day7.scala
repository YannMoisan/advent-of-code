object Day7 extends MultiPuzzle[Int, Int] {
  override def part1(lines: Iterator[String]): Int = lines.count(isTLS)

  override def part2(lines: Iterator[String]): Int = lines.count(isSSL)

  def isABBA(s: String) = s(0) == s(3) && s(1) == s(2) && s(0) != s(1)

  def containsABBA(s: String) = s.sliding(4).exists(isABBA)

  def abas2(s: String): Array[String] = s.sliding(3).filter(isABA).toArray

  def containsBAB(s: String, babs: Seq[String]) = babs.exists(s.contains)

  def split(s: String): Array[String] = s.split("""[\[\]]""")

  def isTLS(s: String) = {
    val (outsides, insides) = partitionByIndex(split(s))
    !insides.exists(containsABBA) && outsides.exists(containsABBA)
  }

  def isABA(s: String) = s(0) == s(2) && s(0) != s(1)

  def BAB(s: String) = List(s(1), s(0), s(1)).mkString

  def isSSL(s: String) = {
    val (outsides, insides) = partitionByIndex(split(s))
    val abas                = outsides.flatMap(abas2)
    if (abas.isEmpty) false else insides.exists(s => containsBAB(s, abas.map(BAB)))
  }

  def partitionByIndex[A](s: Seq[A]): (Seq[A], Seq[A]) = {
    val (a, b) = s.zipWithIndex.partition { case (_, i) => i % 2 == 0 }
    (a.map(_._1), b.map(_._1))
  }
}
