import com.yannmoisan.util.geo.Position
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
    val grid = Grid1D(input)

    val sparseGrid: Set[Position] = grid.dim.indices.collect {
      case i if grid(i) == '#' =>
        val p = grid.dim.pos(i)
        Position(p.x, p.y)
    }.toSet

    val end = Iterator.iterate((sparseGrid, 0, -1))(next).drop(10).next()
    count(end._1)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)

    val sparseGrid: Set[Position] = grid.dim.indices.collect {
      case i if grid(i) == '#' =>
        val p = grid.dim.pos(i)
        Position(p.x, p.y)
    }.toSet

    val it = Iterator.iterate((sparseGrid, 0, -1))(next)
    it.zipWithIndex.find(_._1._3 == 0).get._2
  }
  //    val grid = enlarge(Grid1D(input), 500)
//
//    val it = Iterator.iterate((grid, 0))(next)
//    firstDuplicateIndex(it).get

  private def next(tuple: (Set[Position], Int, Int)): (Set[Position], Int, Int) = {
    val moveProposal: Set[(Position, Position)] = tuple._1.flatMap { pos =>
      if (Direction8.all.count(dir => tuple._1.contains(Position.move(pos, dir))) > 0) {
        val prop = (0 to 3)
          .map(i => (i + tuple._2) % 4)
          .find(j => propositions(j)._1.forall(dir => !tuple._1.contains(Position.move(pos, dir))))

        prop.map(pp => (Position.move(pos, propositions(pp)._2), pos))
      } else {
        None
      }
    }

    val selectedProposal: Map[Position, Set[Position]] =
      moveProposal.groupMap(_._1)(_._2).filter(_._2.size == 1)
    (
      tuple._1.removedAll(selectedProposal.values.flatten) ++ selectedProposal.keys,
      tuple._2 + 1,
      selectedProposal.keys.size
    )
  }

//  private def enlarge(grid: Grid[Char], size: Int): Grid[Char] =
//    Grid1D.tabulate(Dimension(grid.dim.width + 2 * size, grid.dim.height + 2 * size)) { i =>
//      val pos = Dimension(grid.dim.width + 2 * size, grid.dim.height + 2 * size).pos(i)
//      if (pos.x < size || pos.x >= size + grid.dim.width || pos.y < size || pos.y >= grid.dim.height + size)
//        '.'
//      else
//        grid(Pos(pos.x - size, pos.y - size))
//    }

  private def count(grid: Set[Position]): Int = {
    val minX = grid.minBy(_.x).x
    val maxX = grid.maxBy(_.x).x
    val minY = grid.minBy(_.y).y
    val maxY = grid.maxBy(_.y).y

    (for {
      x <- minX to maxX
      y <- minY to maxY
      if !grid.contains(Position(x, y))
    } yield (x, y)).size
  }
}
