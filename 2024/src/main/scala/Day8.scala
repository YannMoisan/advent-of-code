import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid       = Grid1D(input)
    val chars      = grid.dim.indices.map(grid(_)).toSet.-('.')
    val charAndPos = chars.map(c => (c, grid.findAll(c)))
    charAndPos.flatMap { case (_, indices) => computeAntiNodes(grid, indices.toList) }.size
  }

  private def computeAntiNodes(grid: Grid[Char], all: List[Int]): List[Int] =
    all.combinations(2).flatMap(l => computeAntiNodes(grid, l(0), l(1))).toList

  private def computeAntiNodes(grid: Grid[Char], a: Int, b: Int): List[Int] = {
    val posA = grid.dim.pos(a)
    val posB = grid.dim.pos(b)
    val dx   = posA.x - posB.x
    val dy   = posA.y - posB.y
    val fstX = posA.x + dx
    val fstY = posA.y + dy
    val sndX = posB.x - dx
    val sndY = posB.y - dy

    List(
      if (fstX >= 0 && fstX < grid.dim.width && fstY >= 0 && fstY < grid.dim.height)
        Some(Pos(fstX, fstY))
      else None,
      if (sndX >= 0 && sndX < grid.dim.width && sndY >= 0 && sndY < grid.dim.height)
        Some(Pos(sndX, sndY))
      else None
    ).flatten.map(grid.dim.index)
  }

  private def computeAntiNodes2(grid: Grid[Char], all: List[Int]): List[Int] =
    all.combinations(2).flatMap(l => computeAntiNodes2(grid, l(0), l(1))).toList

  private def computeAntiNodes2(grid: Grid[Char], a: Int, b: Int): List[Int] = {
    val posA = grid.dim.pos(a)
    val posB = grid.dim.pos(b)
    val dx   = posA.x - posB.x
    val dy   = posA.y - posB.y
    (unfold(grid, posA, dx, dy) ++ unfold(grid, posB, -dx, -dy)).map(grid.dim.index)
  }

  private def unfold(grid: Grid[Char], init: Pos, dx: Int, dy: Int): List[Pos] =
    Iterator
      .unfold(init) { (p: Pos) =>
        val newX = p.x + dx
        val newY = p.y + dy
        val newPos =
          if (newX >= 0 && newX < grid.dim.width && newY >= 0 && newY < grid.dim.height)
            Some(Pos(newX, newY))
          else None
        (newPos.map(p => (p, p)))
      }.toList

  override def part2(input: Iterator[String]): Int = {
    val grid       = Grid1D(input)
    val chars      = grid.dim.indices.map(grid(_)).toSet.-('.')
    val charAndPos = chars.map(c => (c, grid.findAll(c)))
    charAndPos.flatMap {
      case (_, indices) => computeAntiNodes2(grid, indices.toList) ++ charAndPos.flatMap(_._2)
    }.size
  }
}
