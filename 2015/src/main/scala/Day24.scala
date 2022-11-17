object Day24 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val total = input.toList.map(_.toLong)
    println(total.sum/4)
    println(total.combinations(2).filter(_.sum == 508).size)
    println(total.combinations(3).filter(_.sum == 508).size)
    println(total.combinations(4).filter(_.sum == 508).size)
    println(total.combinations(5).filter(_.sum == 508).size)
    println(total.combinations(6).filter(_.sum == 508).size)
    println(total.combinations(7).filter(_.sum == 508).size)
    total.combinations(6).filter(_.sum == 508).minBy(_.product).product
  }

  override def part2(input: Iterator[String]): Long = {
    val total = input.toList.map(_.toLong)
    println(total.combinations(2).filter(_.sum == 381).size)
    println(total.combinations(3).filter(_.sum == 381).size)
    println(total.combinations(4).filter(_.sum == 381).size)
    println(total.combinations(5).filter(_.sum == 381).size)
    println(total.combinations(6).filter(_.sum == 381).size)
    println(total.combinations(7).filter(_.sum == 381).size)
    total.combinations(5).filter(_.sum == 381).minBy(_.product).product
  }
}
