object Day13 extends MultiPuzzle[Int, Int] {

  def makeGrid(lines: Array[String]): Array[Array[Char]] = {
    val points = lines.map { line =>
      val Array(x, y) = line.split(",")
      (x.toInt, y.toInt)
    }.toArray

    val mx = points.maxBy(_._1)._1
    val my = points.maxBy(_._2)._2

    println(s"mx=${mx + 1}, my=${my + 1}")

    val arr = Array.fill(my + 2, mx + 1)('.')
    points.foreach { case (x, y) => arr(y)(x) = '#' }

    arr
  }

  def part1(input: Iterator[String]): Int = {
    val grid = makeGrid(input.toArray)
    val g1   = foldX(grid)
    val g2   = foldY(g1)
    val g3   = foldX(g2)
    val g4   = foldY(g3)
    val g5   = foldX(g4)
    val g6   = foldY(g5)
    val g7   = foldX(g6)
    val g8   = foldY(g7)
    val g9   = foldX(g8)
    val g10  = foldY(g9)
    val g11  = foldY(g10)
    val g12  = foldY(g11)
    g12.foreach(l => println(l.mkString))
    g1.map(_.count(_ == '#')).sum
  }

  def part2(input: Iterator[String]): Int = {
    assert(input == input)
    //"BLKJRBAG"
    14
  }

  def foldY(grid: Array[Array[Char]]): Array[Array[Char]] = {
    println(s"foldY:${grid.length} -> ${grid.length / 2}")
    val newGrid = Array.fill(grid.length / 2, grid.head.length)('.')

    for {
      x <- 0 until grid.head.length
      y <- 0 until grid.length / 2
    } {
      newGrid(y)(x) = if (grid(y)(x) == '.' && grid(grid.length - y - 1)(x) == '.') '.' else '#'
    }

    newGrid
  }

  def foldX(grid: Array[Array[Char]]): Array[Array[Char]] = {
    println(s"foldX:${grid.head.length} -> ${grid.head.length / 2}")
    val newGrid = Array.fill(grid.length, grid.head.length / 2)('.')

    for {
      x <- 0 until grid.head.length / 2
      y <- 0 until grid.length
    } {
      newGrid(y)(x) =
        if (grid(y)(x) == '.' && grid(y)(grid.head.length - x - 1) == '.') '.' else '#'
    }

    newGrid
  }
}
