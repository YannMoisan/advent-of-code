object Day1 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = input.foldLeft(0) {
    case (acc, i) => if (i == '(') acc + 1 else acc - 1
  }

  override def part2(input: String): Int = {
    var i     = 0
    var floor = 0
    while (i < input.length && floor != -1) {
      if (input.charAt(i) == '(') floor += 1 else floor -= 1
      i += 1
    }
    i
  }
}
