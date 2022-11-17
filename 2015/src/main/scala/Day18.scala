object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = input.toArray.map(_.toArray)
    val end  = (1 to 100).foldLeft(grid) { case (acc, _) => next(acc) }
    end.map(_.count(_ == '#')).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = input.toArray.map(_.toArray)
    grid(0)(0) = '#'
    grid(0)(99) = '#'
    grid(99)(0) = '#'
    grid(99)(99) = '#'

    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) =>
        val res = next(acc)
        res(0)(0) = '#'
        res(0)(99) = '#'
        res(99)(0) = '#'
        res(99)(99) = '#'
        res
    }

    end.map(_.count(_ == '#')).sum
  }

  def next(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val res = Array.ofDim[Char](grid.size, grid.head.size)
    for {
      row <- 0 until res.size
      col <- 0 until res.head.size
    } {
      val neighbors: Seq[(Int, Int)] = for {
        nrow <- row - 1 to row + 1
        ncol <- col - 1 to col + 1
        if nrow >= 0 && nrow < grid.size && ncol >= 0 && ncol < grid.head.size
        if nrow != row || ncol != col
      } yield (nrow, ncol)

      val neighborsOn = neighbors.count { case (row2, col2) => grid(row2)(col2) == '#' }
      val newState: Char = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
      res(row)(col) = newState
    }
    res
  }
}
