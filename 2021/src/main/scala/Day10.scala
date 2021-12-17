object Day10 extends MultiPuzzle[Int, Long] {
  private val matchings = Map('>' -> '<', ']' -> '[', '}' -> '{', ')'  -> '(')
  private val scores    = Map(')' -> 3, ']'   -> 57, '}'  -> 1197, '>' -> 25137)
  private val scores2   = Map('(' -> 1, '['   -> 2, '{'   -> 3, '<'    -> 4)

  override def part1(input: Iterator[String]): Int =
    input.map(parse).map(_._2).sum

  override def part2(input: Iterator[String]): Long = {
    val scores: Array[Long] =
      input
        .map(parse).filter(_._2 == 0).map(res =>
          res._1.foldLeft(0L) { case (acc, c) => acc * 5 + scores2(c) }
        ).toArray
    val sorted = scores.sorted
    sorted(scores.size / 2)
  }

  private def parse(line: String): (List[Char], Int) =
    line
      .foldLeft((List.empty[Char], 0)) {
        case (acc, c) =>
          if (acc._2 == 0) {
            if (Seq('[', '(', '{', '<').contains(c))
              (c :: acc._1, 0)
            else {
              acc._1 match {
                case h :: t if h == matchings(c) => (t, 0)
                case _                           => (acc._1, scores(c))
              }
            }
          } else acc
      }
}
