import com.yannmoisan.util.grid.{Grid, Grid1D}

import scala.collection.mutable

object Day10 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    //.map(_.toString.toInt)
    val starts = grid.findAll('0')
    //val classic = new StandardMove(grid.dim) {}
    starts.map { start =>
      shortestPath(
        grid,
        start,
        '9',
        (from, to) => {
          val diff = grid(to).toString.toInt - grid(from).toString.toInt
          diff == 1
        }
      )
    }.sum
  }
  def shortestPath(
      grid: Grid[Char],
      start: Int,
      target: Char,
      isValid: (Int, Int) => Boolean
  ): Int = {
    val q       = mutable.Queue[List[Int]]()
    val visited = Array.ofDim[Boolean](grid.dim.width * grid.dim.height)
    q.enqueue(start :: Nil)
    //var targetedPath: Option[List[Int]] = None
    var count = 0
    while (q.nonEmpty) {
      val path = q.dequeue()
      val pos  = path.head
      val arr  = grid.dim.neighbors4(pos)
      // PERF: while loop is faster than array.foreach
      var i = 0
      while (i < arr.length) {
        val newPos = arr(i)
        if (!visited(newPos) && isValid(pos, newPos)) {
          if (grid(newPos) == target)
            count += 1
          q.enqueue(newPos :: path)
          visited(newPos) = true
        }
        i += 1
      }
    }
    count
  }

  def shortestPath2(
      grid: Grid[Char],
      start: Int,
      target: Char,
      isValid: (Int, Int) => Boolean
  ): Int = {
    val q       = mutable.Queue[List[Int]]()
    val visited = Array.ofDim[Boolean](grid.dim.width * grid.dim.height)
    q.enqueue(start :: Nil)
    //var targetedPath: Option[List[Int]] = None
    var count = 0
    while (q.nonEmpty) {
      val path = q.dequeue()
      val pos  = path.head
      val arr  = grid.dim.neighbors4(pos)
      // PERF: while loop is faster than array.foreach
      var i = 0
      while (i < arr.length) {
        val newPos = arr(i)
        if (isValid(pos, newPos)) {
          if (grid(newPos) == target)
            count += 1
          q.enqueue(newPos :: path)
          visited(newPos) = true
        }
        i += 1
      }
    }
    count
  }
  // 128 < r < 1351

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    //.map(_.toString.toInt)
    val starts = grid.findAll('0')
    //val classic = new StandardMove(grid.dim) {}
    starts.map { start =>
      shortestPath2(
        grid,
        start,
        '9',
        (from, to) => {
          val diff = grid(to).toString.toInt - grid(from).toString.toInt
          diff == 1
        }
      )
    }.sum
  }
}
