object Day4 extends SinglePuzzle[Int, Int] {
  override def part1: String => Int = { _ =>
    (137683 to 596253)
      .map(_.toString.toArray.sliding(2).toArray)
      .filter(
        _.forall { case Array(a, b) => a <= b }
      )
      .filter(
        _.exists { case Array(a, b) => a == b }
      )
      .size
  }

  override def part2: String => Int = { _ =>
    (137683 to 596253)
      .filter { i =>
        i.toString.toArray.sliding(2).toArray.forall { case Array(a, b) => a <= b }
      }
      .filter { i =>
        val s = i.toString
        (0 until 5).exists { i =>
          s(i) == s(i + 1) && (i == 0 || s(i - 1) != s(i)) && (i == 4 || s(i + 2) != s(i + 1))
        }
      }
      .size
  }
}
