object Day13 extends MultiPuzzle[Int, Int] {
  override def part1(lines: Iterator[String]) : Int = {
    val a = lines.map(_.split(": "))
    val b = a.map { t =>
      (t(0).toInt, t(1).toInt)
    }.toSeq

    val max = b.maxBy(_._1)._1
    val m: Map[Int, Int] = b.toMap

    (0 to max).map { i =>
      m.get(i)
        .map { range =>
          if (i % ((range - 1) * 2) == 0)
            i * m(i)
          else
            0
        }
        .getOrElse(0)
    }.sum
  }

  override def part2(lines: Iterator[String]) : Int = {
    val a = lines.map(_.split(": "))
    val b = a.map { t =>
      (t(0).toInt, t(1).toInt)
    }.toSeq

    val max = b.maxBy(_._1)._1
    val m: Map[Int, Int] = b.toMap

    val nbs = (0 to 10000000).map { nb =>
      (0 to max).map { i =>
        m.get(i)
          .map { range =>
            if ((i + nb) % ((range - 1) * 2) == 0)
              1 //i * m(i)
            else
              0
          }
          .getOrElse(0)
      }.sum
    }
    nbs.indexOf(0)
  }
}
