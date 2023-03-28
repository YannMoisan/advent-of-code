import com.yannmoisan.util.collection.firstDuplicate
import com.yannmoisan.util.grid.{Grid, Grid1D}

object Day24 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val it               = Iterator.iterate(grid)(next)
    val it2 = it.map { g =>
      var sum = 0
      grid.dim.indices.foreach(i => if (g(i) == '#') sum += math.pow(2, i.toDouble).toInt)
      sum

    }
    firstDuplicate(it2).get

//    val end              = firstDuplicate(it).get
//    var sum              = 0
//    grid.dim.indices.foreach(i => if (end(i) == '#') sum += math.pow(2, i.toDouble).toInt)
//    sum
  }

  /*
      A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
      An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
   */
  private def next(grid: Grid[Char]): Grid[Char] =
    Grid1D.tabulate(grid.dim) { i =>
      val n4: Array[Int] = grid.dim.neighbors4(i)
      grid(i) match {
        case '#' if n4.count(grid(_) == '#') == 1                => '#'
        case '#'                                                 => '.'
        case '.' if Set(1, 2).contains(n4.count(grid(_) == '#')) => '#'
        case '.'                                                 => '.'
      }
    }

  override def part2(input: Iterator[String]): Int = 42
}
