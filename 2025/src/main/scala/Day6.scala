object Day6 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val operations: Seq[Array[String]] = input.map(_.split("[ ]+")).toVector
    operations.head.indices.map { i =>
      val numbers = (0 to 3).map(row => operations(row)(i).toLong)
      if (operations(4)(i) == "*")
        numbers.reduce(_ * _)
      else
        numbers.reduce(_ + _)
    }.sum
  }

  // read column by column
  override def part2(input: Iterator[String]): Long = {
    val lines: Array[String]        = input.toArray
    var startNewBlock               = true
    var block: List[Long]           = Nil
    var curOp: (Long, Long) => Long = null
    var sum                         = 0L
    (0 until lines.head.length).foreach { col =>
      if (startNewBlock)
        if (lines(4)(col) == '*')
          curOp = _ * _
        else
          curOp = _ + _
      val read: Seq[Char] = (0 to 3).map(row => lines(row)(col))
      if (read.exists(_ != ' ')) {
        startNewBlock = false
        block = read.filter(_ != ' ').mkString.toLong :: block
      } else {
        sum += block.reduce(curOp)
        startNewBlock = true
        block = Nil
      }
    }
    sum += block.reduce(curOp)
    sum
  }
}
