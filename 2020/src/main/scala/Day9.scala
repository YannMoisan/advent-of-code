object Day9 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val numbers = input.map(_.toLong).toArray
    var i       = 25
    var found   = false
    while (!found) {
      found = twoSum(numbers.slice(i - 25, i).toSet, numbers(i)).isEmpty
      i += 1
    }
    numbers(i - 1)
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def part2(input: Iterator[String]): Long = {
    var i       = 0
    var j       = 0
    val numbers = input.map(_.toLong).toArray
    var found   = false
    while (!found) {
      j = i + 1
      var sum = numbers(i)
      while (!found && sum < 542529149L) {
        sum += numbers(j)
        if (sum == 542529149L)
          found = true
        else
          j += 1
      }
      if (!found)
        i += 1
    }
    val contiguous = numbers.slice(i, j + 1)
    contiguous.min + contiguous.max
  }

  def twoSum(set: Set[Long], target: Long): Option[(Long, Long)] =
    set.find(x => set.contains(target - x)).map(x => (x, target - x))

}
