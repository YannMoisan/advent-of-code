// permutation
object Day13 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val units: Map[(String, String), Int] = input.map { s =>
      val s"$a would $loseOrGain $unit happiness units by sitting next to $b." = s
      (a, b) -> (if (loseOrGain == "gain") unit.toInt else -unit.toInt)
    }.toMap

    val names = List("Bob", "Carol", "David", "Eric", "Frank", "George", "Mallory")
    names.permutations.map { perm =>
      var total = 0

      (("Alice" :: perm) :+ "Alice").sliding(2).foreach {
        case List(a, b) =>
          total += units((a, b)) + units((b, a))
        case _ =>
      }
      total
    }.max
  }

  override def part2(input: Iterator[String]): Int = {
    val units: Map[(String, String), Int] = input.map { s =>
      val s"$a would $loseOrGain $unit happiness units by sitting next to $b." = s
      (a, b) -> (if (loseOrGain == "gain") unit.toInt else -unit.toInt)
    }.toMap

    val names = List("Bob", "Carol", "David", "Eric", "Frank", "George", "Mallory", "Myself")
    names.permutations.map { perm =>
      var total = 0

      (("Alice" :: perm) :+ "Alice").sliding(2).foreach {
        case List(a, b) =>
          total += units.getOrElse((a, b), 0) + units.getOrElse((b, a), 0)
        case _ =>
      }
      total
    }.max
  }
}
