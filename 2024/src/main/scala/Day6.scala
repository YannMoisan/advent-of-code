import com.yannmoisan.util.grid.{Direction4, Grid1D, StandardMove}

import scala.collection.mutable

// oublié comment marche ma lib
// Bonne réponse du premier coup

object Day6 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid                = Grid1D(input)
    val guard               = Guard(Direction4.Up, grid.find('^').get)
    val classic             = new StandardMove(grid.dim) {}
    var next: Option[Guard] = Some(guard)
    val visited             = mutable.Set[Int]()
    visited.add(guard.pos)
    while (next.isDefined) {
      classic.move(next.get.pos, next.get.dir) match {
        case Some(newPos) if grid(newPos) == '#' =>
          next = Some(
            Guard(Direction4.all((Direction4.all.indexOf(next.get.dir) + 1) % 4), next.get.pos)
          )
        case Some(newPos) => next = Some(Guard(next.get.dir, newPos))
        case None         => next = None
      }
      next.foreach(g => visited.add(g.pos))
    }
    visited.size
  }

  case class Guard(dir: Direction4, pos: Int)

  override def part2(input: Iterator[String]): Int = {
    val grid                = Grid1D(input)
    val guard               = Guard(Direction4.Up, grid.find('^').get)

    grid.dim.indices
      .filter { i => grid(i) != '#' && grid(i) != '^' }
      .filter { i =>
        grid(i) = '#'
        val res = isLoop(grid, guard)
        grid(i) = '.'
        res
      }.size
  }

  def isLoop(grid: Grid1D[Char], guard: Guard): Boolean = {
    val classic             = new StandardMove(grid.dim) {}
    var next: Option[Guard] = Some(guard)
    val visited             = mutable.Set[Int]()
    visited.add(guard.pos)
    var i = 0
    while (next.isDefined && i < 10000) {
      i += 1
      classic.move(next.get.pos, next.get.dir) match {
        case Some(newPos) if grid(newPos) == '#' =>
          next = Some(
            Guard(Direction4.all((Direction4.all.indexOf(next.get.dir) + 1) % 4), next.get.pos)
          )
        case Some(newPos) => next = Some(Guard(next.get.dir, newPos))
        case None         => next = None
      }
      next.foreach(g => visited.add(g.pos))
    }
    i == 10000
  }
}
