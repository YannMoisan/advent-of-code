import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Day9 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = input.toArray
    val maxx = grid.head.length
    val maxy = grid.length

    val tmp = for {
      i <- 0 until maxx
      j <- 0 until maxy
    } yield {
      val neighbors = Seq(
        (i, j + 1),
        (i, j - 1),
        (i + 1, j),
        (i - 1, j)
      ).filter { case (x, y)             => x >= 0 && x < maxx && y >= 0 && y < maxy }
      if (neighbors.forall { case (x, y) => grid(y)(x) > grid(j)(i) })
        Integer.parseInt(grid(j)(i).toString) + 1
      else 0
    }
    tmp.sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = input.toArray
    val maxx = grid.head.length
    val maxy = grid.length

    val tmp: Seq[Option[(Int, Int)]] = for {
      i <- 0 until maxx
      j <- 0 until maxy
    } yield {
      val neighbors = Seq(
        (i, j + 1),
        (i, j - 1),
        (i + 1, j),
        (i - 1, j)
      ).filter { case (x, y)             => x >= 0 && x < maxx && y >= 0 && y < maxy }
      if (neighbors.forall { case (x, y) => grid(y)(x) > grid(j)(i) })
        Some((i, j))
      else None
    }
    tmp.flatten.map(floodFill(grid, _)).sortBy(x => -x).take(3).product
  }

  def floodFill(g: Array[String], start: (Int, Int)): Int = {
    val q       = mutable.Queue[(Int, Int)]()
    val visited = mutable.Set[(Int, Int)]()
    val _       = q.enqueue(start)
    var size    = 0
    while (!q.isEmpty) {
      val (i, j) = q.dequeue()
      if (!visited.contains((i, j))) {
        size += 1
        val _ = visited.add((i, j))
        val neighbors = Seq(
          (i, j + 1),
          (i, j - 1),
          (i + 1, j),
          (i - 1, j)
        ).filter { case (x, y) => x >= 0 && x < g.head.length && y >= 0 && y < g.length }
        neighbors.foreach {
          case (x, y) =>
            if (g(y)(x).toString.toInt > g(j)(i).toString.toInt && g(y)(x).toString.toInt < 9 && !visited
                  .contains((x, y))) {
              val _ = q.enqueue((x, y))
            }
        }
      }
    }
    size
  }

}
