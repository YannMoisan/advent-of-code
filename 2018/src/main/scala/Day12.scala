object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val s"initial state: $initialState" = input.next()
    val init                            = initialState.toCharArray
    val expanded                        = Array.fill(40)('.') ++ init ++ Array.fill(40)('.')

    val rules: Map[String, Char] = input
      .drop(1).take(32).map {
        case s"$from => $to" => (from, to.head)
      }.toMap

    val end: Array[Char] = Iterator.iterate(expanded)(update(_, rules)).drop(20).next()
    end.zipWithIndex.map { case (ch, i) => if (ch == '#') i - 40 else 0 }.sum
  }

  def update(arr: Array[Char], rules: Map[String, Char]): Array[Char] = {
    val next = arr.clone()
    (0 to arr.size - 5).foreach(i => next(i + 2) = rules(arr.slice(i, i + 5).mkString))
    next
  }

  override def part2(input: Iterator[String]): Int =
    43
}
