import com.yannmoisan.util.graph.BFS
import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

import scala.collection.mutable

object Day24 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    println(grid.dim)

    // 0 : v
    // 1 : ^
    // 2 : >
    // 3 : <
    val tmp = Array.fill(4)(mutable.Set[Pos]())
    grid.dim.indices.foreach { i =>
      val pos = grid.dim.pos(i)
      if (grid(i) == 'v')
        tmp(0).add(pos)
      if (grid(i) == '^')
        tmp(1).add(pos)
      if (grid(i) == '>')
        tmp(2).add(pos)
      if (grid(i) == '<')
        tmp(3).add(pos)
    }
    val blizzards: Array[Set[Pos]] = tmp.map(_.toSet)

    val start = (Pos(1, 0), 0)

    val allBlizzards: Array[Array[Set[Pos]]] = Array.iterate(blizzards, 10000)(simulate)

    // visited are used because same pos at the same time will give the same subgraph
    BFS
      .breadth_first_traverse_no_path_it(start, next(grid, allBlizzards)).find(
        _._1 == Pos(100, 36)
      ).get._2
  }

  private def simulate(blizzards: Array[Set[Pos]]): Array[Set[Pos]] =
    Array.tabulate(4) {
      case 0 => blizzards(0).map(p => if (p.y == 35) Pos(p.x, 1) else Pos(p.x, p.y + 1))  // v
      case 1 => blizzards(1).map(p => if (p.y == 1) Pos(p.x, 35) else Pos(p.x, p.y - 1))  // ^
      case 2 => blizzards(2).map(p => if (p.x == 100) Pos(1, p.y) else Pos(p.x + 1, p.y)) // >
      case 3 => blizzards(3).map(p => if (p.x == 1) Pos(100, p.y) else Pos(p.x - 1, p.y)) // <
    }

  private def isEmpty(blizzards: Array[Set[Pos]], pos: Pos): Boolean =
    blizzards.forall(!_.contains(pos))

  private def next(grid: Grid[Char], allBlizzards: Array[Array[Set[Pos]]])(
      e: (Pos, Int)
  ): Seq[(Pos, Int)] = {
    val (p, length) = e
    val candidates  = grid.dim.neighbors4(grid.dim.index(p)).map(grid.dim.pos) :+ p
    candidates
      .filter(grid(_) != '#')
      .filter(p => isEmpty(allBlizzards(length + 1), p))
      .map(p => (p, length + 1))
      .toIndexedSeq
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    println(grid.dim)

    // 0 : v
    // 1 : ^
    // 2 : >
    // 3 : <
    val tmp = Array.fill(4)(mutable.Set[Pos]())
    grid.dim.indices.foreach { i =>
      val pos = grid.dim.pos(i)
      if (grid(i) == 'v')
        tmp(0).add(pos)
      if (grid(i) == '^')
        tmp(1).add(pos)
      if (grid(i) == '>')
        tmp(2).add(pos)
      if (grid(i) == '<')
        tmp(3).add(pos)
    }
    val blizzards: Array[Set[Pos]] = tmp.map(_.toSet)

    val start = (Pos(1, 0), 0)

    val allBlizzards: Array[Array[Set[Pos]]] = Array.iterate(blizzards, 10000)(simulate)

    // visited are used because same pos at the same time will give the same subgraph
    val fst =
      BFS
        .breadth_first_traverse_no_path_it(start, next(grid, allBlizzards)).find(
          _._1 == Pos(100, 36)
        ).get
    val snd = BFS
      .breadth_first_traverse_no_path_it((Pos(100, 36), fst._2), next(grid, allBlizzards)).find(
        _._1 == Pos(1, 0)
      ).get
    BFS
      .breadth_first_traverse_no_path_it((Pos(1, 0), snd._2), next(grid, allBlizzards)).find(
        _._1 == Pos(100, 36)
      ).get._2

  }
}
