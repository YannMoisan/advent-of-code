object Day17 extends MultiPuzzle[Int, Int] {
  case class Position3D(x: Int, y: Int, z: Int)
  case class Grid3D(actives: Set[Position3D]) {
    def activeNeighbors(p: Position3D): Int = {
      val neighbors = for {
        x <- p.x - 1 to p.x + 1
        y <- p.y - 1 to p.y + 1
        z <- p.z - 1 to p.z + 1
        if !(x == p.x && y == p.y && z == p.z)
      } yield Position3D(x, y, z)
      actives.intersect(neighbors.toSet).size
    }
  }
  case class Position4D(x: Int, y: Int, z: Int, t: Int)
  case class Grid4D(actives: Set[Position4D]) {
    def activeNeighbors(p: Position4D): Int = {
      val neighbors = for {
        x <- p.x - 1 to p.x + 1
        y <- p.y - 1 to p.y + 1
        z <- p.z - 1 to p.z + 1
        t <- p.t - 1 to p.t + 1
        if !(x == p.x && y == p.y && z == p.z && t == p.t)
      } yield Position4D(x, y, z, t)
      actives.intersect(neighbors.toSet).size
    }
  }

  override def part1(input: Iterator[String]): Int = {
    val arr = input.toArray
    val actives = for {
      y <- 0 until arr.length
      x <- 0 until arr.head.length
      if arr(y)(x) == '#'
    } yield { Position3D(x, y, 0) }
    val grid = Grid3D(actives.toSet)
    Iterator.iterate(grid)(nextP1).drop(6).next().actives.size
  }

  private def nextP1(g: Grid3D): Grid3D = {
    val actives = for {
      x <- g.actives.minBy(_.x).x - 1 to g.actives.maxBy(_.x).x + 1
      y <- g.actives.minBy(_.y).y - 1 to g.actives.maxBy(_.y).y + 1
      z <- g.actives.minBy(_.z).z - 1 to g.actives.maxBy(_.z).z + 1
      pos   = Position3D(x, y, z)
      count = g.activeNeighbors(pos)
      if (g.actives.contains(pos) && (count == 2 || count == 3)) || (!g.actives.contains(pos) && count == 3)
    } yield pos
    Grid3D(actives.toSet)
  }

  override def part2(input: Iterator[String]): Int = {
    val arr = input.toArray
    val actives = for {
      y <- 0 until arr.length
      x <- 0 until arr.head.length
      if arr(y)(x) == '#'
    } yield { Position4D(x, y, 0, 0) }
    val grid = Grid4D(actives.toSet)
    Iterator.iterate(grid)(nextP2).drop(6).next().actives.size
  }

  private def nextP2(g: Grid4D): Grid4D = {
    val actives = for {
      x <- g.actives.minBy(_.x).x - 1 to g.actives.maxBy(_.x).x + 1
      y <- g.actives.minBy(_.y).y - 1 to g.actives.maxBy(_.y).y + 1
      z <- g.actives.minBy(_.z).z - 1 to g.actives.maxBy(_.z).z + 1
      t <- g.actives.minBy(_.t).t - 1 to g.actives.maxBy(_.t).t + 1
      pos   = Position4D(x, y, z, t)
      count = g.activeNeighbors(pos)
      if (g.actives.contains(pos) && (count == 2 || count == 3)) || (!g.actives.contains(pos) && count == 3)
    } yield pos
    Grid4D(actives.toSet)
  }
}
