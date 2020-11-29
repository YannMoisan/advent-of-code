object Day10 extends MultiPuzzle[Int, Int] {
  override def part1: Iterator[String] => Int = { _ =>
    val line: Iterator[String] = """.#..#
      |.....
      |#####
      |....#
      |...##""".stripMargin.split('\n').iterator
    val arr: Array[String]     = line.toArray
    //.foreach(println)
    // find all combinations
    val pos: Seq[(Int, Int)] = for {
      i <- 0 until arr(0).length
      j <- 0 until arr.size
      if arr(i)(j) == '#'
    } yield (i, j)
    println(s"# pos=${pos.size}")
    println(s"pos=${pos.mkString(",")}")
    val comb = pos.combinations(2).toArray
    println(s"# comb=${comb.size}")
    comb.map { seq =>
      val Array((ax, ay), (bx, by)) = seq.toArray
      val dx                        = ax - bx
      val dy                        = ay - by
      var ret                       = 0
      if (dy == 0 && dx > 0 && (1 until dx).forall(i => arr(ax + i)(ay) != '#'))
        ret += 1
      if (dy == 0 && dx < 0 && (1 until -dx).forall(i => arr(ax - i)(ay) != '#'))
        ret += 1
      if (dx == 0 && dy > 0 && (1 until dy).forall(i => arr(ax)(ay + i) != '#'))
        ret += 1
      if (dx == 0 && dy < 0 && (1 until -dy).forall(i => arr(ax)(ay - i) != '#'))
        ret += 1
      if (dx != 0 && dy != 0) {
        val div  = math.abs(gcd(dx, dy))
        val incx = dx / div
        val incy = dy / div
        if ((1 until div).forall(i => arr(ax + i * incx)(ay + i * incy) != '#'))
          ret += 1
      }
      ret
    }.max
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  override def part2: Iterator[String] => Int = ???
}
