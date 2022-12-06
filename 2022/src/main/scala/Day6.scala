object Day6 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    var i = 3
    while (i < input.length && (i-3 to i).map(input(_)).toSet.size != 4) {
      i +=1
    }
    i+1
  }

  override def part2(input: String): Int = {
    var i = 13
    while (i < input.length && (i - 13 to i).map(input(_)).toSet.size != 14) {
      i += 1
    }
    i + 1
  }
}
