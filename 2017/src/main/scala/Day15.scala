object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    // Generator A starts with 277
    // Generator B starts with 349
    val startA = 277
    val startB = 349

    val genA    = Iterator.iterate(startA)(next(_, 16807))
    val genB    = Iterator.iterate(startB)(next(_, 48271))
    val genPair = genA.zip(genB)

    genPair.take(40_000_000).count { case (a, b) => a.toShort == b.toShort }
  }

  override def part2(input: Iterator[String]): Int = {
    // Generator A starts with 277
    // Generator B starts with 349
    val startA = 277
    val startB = 349

    val genA    = Iterator.iterate(startA)(next(_, 16807)).filter(_ % 4 == 0)
    val genB    = Iterator.iterate(startB)(next(_, 48271)).filter(_ % 8 == 0)
    val genPair = genA.zip(genB)

    genPair.take(5_000_000).count { case (a, b) => a.toShort == b.toShort }
  }

  def next(value: Int, factor: Int): Int = ((value.toLong * factor) % 2147483647).toInt
}
