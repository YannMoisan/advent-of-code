@SuppressWarnings(
  Array(
    "org.wartremover.warts.OptionPartial"
  )
)
object Day20 extends MultiPuzzle[Int, Int] {
  // TODO is there a clever solution ?
  override def part1(input: Iterator[String]): Int = {
    val Limit = 1000000

    val houses = Array.ofDim[Int](Limit+1)
    for {
      elf <- 1 until Limit
      inc <- elf until Limit by elf
    } {
      houses(inc) = houses(inc) + elf * 10
    }

    (1 until Limit).find(i => houses(i) > 34000000).get
  }

  override def part2(input: Iterator[String]): Int = {
    val Limit = 1000000

    val houses = Array.ofDim[Int](Limit + 1)
    for {
      elf <- 1 until Limit
      inc <- (elf until Limit by elf).take(50)
    } {
      houses(inc) = houses(inc) + elf * 11
    }

    (1 until Limit).find(i => houses(i) > 34000000).get
  }
}