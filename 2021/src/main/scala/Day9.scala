import com.yannmoisan.util.grid.{Grid, Grid1D}

import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Day9 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit)))
    grid.dim.indices.map { i =>
      if (grid.dim.neighbors4(i).forall(j => grid(j) > grid(i)))
        grid(i) + 1
      else 0
    }.sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit)))
    val lowPoints = grid.dim.indices.filter { i =>
      grid.dim.neighbors4(i).forall(j => grid(j) > grid(i))
    }

    lowPoints.map(floodFill(grid, _)).sortBy(x => -x).take(3).product
  }

  def floodFill(g: Grid[Int], start: Int): Int = {
    val q       = mutable.Queue[Int]()
    val visited = mutable.Set[Int]()
    val _       = q.enqueue(start)
    var size    = 0
    while (!q.isEmpty) {
      val i = q.dequeue()
      if (!visited.contains(i)) {
        size += 1
        val _ = visited.add(i)
        g.dim.neighbors4(i).foreach { j =>
          if (g(j) > g(i) && g(j) < 9 && !visited.contains(j)) {
            val _ = q.enqueue(j)
          }
        }
      }
    }
    size
  }

}
