object Day6 extends MultiPuzzle[Int, Int] {
  override def part1: Iterator[String] => Int = { iter =>
    val lines  = iter.toArray
    val pat    = """(\d+), (\d+)""".r
    val points = lines.map { case pat(x, y) => (x.toInt, y.toInt) }
    points.take(5).foreach(println)
    val arr: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    for {
      x <- (0 until 1000)
      y <- (0 until 1000)
    } {
      val sorted = points
        .map { case (px, py) => dist(x, y, px, py) }
        .zipWithIndex
        .sortBy(_._1)
      val closest = if (sorted(0)._1 != sorted(1)._1) sorted(0)._2 else -1
      arr(y)(x) = closest
    }

    val m: Map[Int, Int] = arr.flatten.groupBy(identity).view.mapValues(_.length).toMap
    m.toSeq.sortBy(-_._2)

    val border: Seq[Int] = (0 until 1000).map { i =>
      Seq(arr(0)(i), arr(i)(0), arr(999)(i), arr(i)(999))
    }.flatten
    println(border.distinct)
    val infinites = border.distinct
    val m2 = m
      .filterNot({ case (id, _) => infinites.contains(id) })
      .toSeq
      .sortBy(-_._2)
    println(m2)
    println(m)
    42
  }

  def dist(x1: Int, y1: Int, x2: Int, y2: Int): Int =
    math.abs(x1 - x2) + math.abs(y1 - y2)

  override def part2: Iterator[String] => Int = { iter =>
    val lines  = iter.toArray
    val pat    = """(\d+), (\d+)""".r
    val points = lines.map { case pat(x, y) => (x.toInt, y.toInt) }
    points.take(5).foreach(println)
    val arr: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    for {
      x <- (0 until 1000)
      y <- (0 until 1000)
    } {
      arr(y)(x) = points.map { case (px, py) => dist(x, y, px, py) }.sum
    }

    arr.flatten.count(_ < 10000)
  }
}
