object Day8 extends MultiPuzzle[Int, Long] {
  override def part1(input: Iterator[String]): Int = {
    val instr = input.next()
    val _     = input.next()
    val a: Map[String, (String, String)] = input.map { s =>
      val s"$src = ($l, $r)" = s
      src -> ((l, r))
    }.toMap

    var i   = 0
    var cur = "AAA"
    while (cur != "ZZZ") {
      cur = if (instr(i % instr.size) == 'L') a(cur)._1 else a(cur)._2
      i += 1
    }
    i
  }

  override def part2(input: Iterator[String]): Long = {
    val instr = input.next()
    val _     = input.next()
    val a: Map[String, (String, String)] = input.map { s =>
      val s"$src = ($l, $r)" = s
      src -> ((l, r))
    }.toMap
    var starts: List[String] = a.keys.filter(_.endsWith("A")).toList
    val firsts: Array[Int]   = Array.ofDim[Int](starts.length)

    var i = 0

    while (firsts.contains(0)) {
      starts = starts.map(cur => if (instr(i % instr.size) == 'L') a(cur)._1 else a(cur)._2)
      i += 1
      starts.indices.foreach(k => if (firsts(k) == 0 && starts(k).endsWith("Z")) firsts(k) = i)
    }
    firsts.map(_.toLong).reduce((a, b) => ppcm(a, b))
  }

  def pgcd(a: Long, b: Long): Long =
    if (b == 0) a else pgcd(b, a % b)

  def ppcm(a: Long, b: Long): Long =
    (a * b) / pgcd(a, b)
}
