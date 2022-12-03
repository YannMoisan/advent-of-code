object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    input.map { case s"$p1 $p2" => (p1, p2) match {
      case ("A", "X") => 1 + 3
      case ("A", "Y") => 2 + 6
      case ("A", "Z") => 3 + 0
      case ("B", "X") => 1 + 0
      case ("B", "Y") => 2 + 3
      case ("B", "Z") => 3 + 6
      case ("C", "X") => 1 + 6
      case ("C", "Y") => 2 + 0
      case ("C", "Z") => 3 + 3
    }}.sum
  }

  override def part2(input: Iterator[String]): Int = {
    input.map { case s"$p1 $p2" => (p1, p2) match {
      case ("A", "X") => 0 + 3
      case ("A", "Y") => 3 + 1
      case ("A", "Z") => 6 + 2
      case ("B", "X") => 0 + 1
      case ("B", "Y") => 3 + 2
      case ("B", "Z") => 6 + 3
      case ("C", "X") => 0 + 2
      case ("C", "Y") => 3 + 3
      case ("C", "Z") => 6 + 1
    }
    }.sum
  }
}
