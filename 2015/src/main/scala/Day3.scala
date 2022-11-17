object Day3 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = positions(input).size

  override def part2(input: String): Int =
    (positions(input.sliding(1, 2).mkString) ++
      positions(input.tail.sliding(1, 2).mkString)).size

  private def positions(input: String): Set[(Int, Int)] =
    input
      .scanLeft((0, 0)) {
        case (pos, '^') => (pos._1, pos._2 - 1)
        case (pos, 'v') => (pos._1, pos._2 + 1)
        case (pos, '<') => (pos._1 - 1, pos._2)
        case (pos, '>') => (pos._1 + 1, pos._2)
      }.toSet
}

//scala> "ABCDEFGHI".sliding(1,2).mkString
//val res1: String = ACEGI
//
//scala> "ABCDEFGHI".tail.sliding(1,2).mkString
//val res2: String = BDFH

//>>> "ABCDEFGHI"[::2]
//'ACEGI'
//>>> "ABCDEFGHI"[1::2]
//'BDFH'
