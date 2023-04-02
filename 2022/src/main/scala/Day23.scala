import com.yannmoisan.util.collection.firstDuplicateIndex
import com.yannmoisan.util.grid._

object Day23 extends MultiPuzzle[Int, Int] {

  val propositions = Seq(
    (Seq(Direction8.Up, Direction8.NE, Direction8.NW), Direction8.Up),
    (Seq(Direction8.Down, Direction8.SE, Direction8.SW), Direction8.Down),
    (Seq(Direction8.Left, Direction8.NW, Direction8.SW), Direction8.Left),
    (Seq(Direction8.Right, Direction8.NE, Direction8.SE), Direction8.Right)
  )

  /*
      If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
      If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
      If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
      If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
   */

  override def part1(input: Iterator[String]): Int = {
    val grid = enlarge(Grid1D(input), 10)

    val end = Iterator.iterate((grid, 0))(next).drop(10).next()
    count(end._1)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = enlarge(Grid1D(input), 500)

    val it = Iterator.iterate((grid, 0))(next)
    firstDuplicateIndex(it).get
  }

  private def next(tuple: (Grid[Char], Int)): (Grid[Char], Int) = {
    println(tuple._2)
    val grid = tuple._1
    val moveProposal: Seq[(Int, Pos)] = {
      (for {
        x <- 1 until grid.dim.width - 1
        y <- 1 until grid.dim.height - 1
      } yield Pos(x, y)).flatMap { pos =>
        val adjCount = grid.dim.neighbors8(grid.dim.index(pos)).count(grid(_) == '#')
        if (grid(pos) == '#' && adjCount != 0) {

          val prop = (0 to 3)
            .map(i => (i + tuple._2) % 4).find { j =>
              propositions(j)._1.forall(dir =>
                grid(grid.dim.moveS(grid.dim.index(pos), dir).get) == '.'
              )
            }
          prop.map(pp => (grid.dim.moveS(grid.dim.index(pos), propositions(pp)._2).get, pos))
        } else
          None
      }
    }
    val ng = grid.copy()

    moveProposal
      .groupMap(_._1)(_._2)
      .filter(_._2.size == 1)
      .foreach {
        case (dst, src) =>
          ng(src.head) = '.'
          ng(dst) = '#'
      }
    (ng, tuple._2 + 1)
  }

  private def enlarge(grid: Grid[Char], size: Int): Grid[Char] =
    Grid1D.tabulate(Dimension(grid.dim.width + 2 * size, grid.dim.height + 2 * size)) { i =>
      val pos = Dimension(grid.dim.width + 2 * size, grid.dim.height + 2 * size).pos(i)
      if (pos.x < size || pos.x >= size + grid.dim.width || pos.y < size || pos.y >= grid.dim.height + size)
        '.'
      else
        grid(Pos(pos.x - size, pos.y - size))
    }

  private def count(grid: Grid[Char]): Int = {
    var minX = 1000
    var maxX = 0
    var minY = 1000
    var maxY = 0
    grid.dim.indices.foreach { i =>
      val pos = grid.dim.pos(i)
      if (grid(pos) == '#') {
        if (pos.x < minX) minX = pos.x
        if (pos.x > maxX) maxX = pos.x
        if (pos.y < minY) minY = pos.y
        if (pos.y > maxY) maxY = pos.y
      }
    }

    (for {
      x <- minX to maxX
      y <- minY to maxY
      if grid(Pos(x, y)) == '.'
    } yield (x, y)).size
  }
}
