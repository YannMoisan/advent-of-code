import com.yannmoisan.util.grid.Grid2D
import com.yannmoisan.util.grid.Pos

import scala.collection.mutable

@SuppressWarnings(
  Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial")
)
object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = new Grid2D(input.toArray.map(_.toArray))
    val s    = grid.find('S').get
    val e    = grid.find('E').get
    length(grid, s, e)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = new Grid2D(input.toArray.map(_.toArray))
    val e    = grid.find('E').get
    (0 to 40).map(y => Pos(0, y)(grid.dim)).map(ss => length(grid, ss, e)).min
  }

  private def length(grid: Grid2D[Char], s: Pos, e: Pos): Int = {
    grid(s) = 'a' // hacky
    grid(e) = 'z' // hacky
    val q       = mutable.Queue[(Pos, Int)]()
    val visited = mutable.Set[Pos]()
    val _       = q.enqueue((s, 0))
    var cur     = (s, 0)
    while (cur._1 != e) {
      cur = q.dequeue()
      val _ = visited += cur._1
      val candidates = Seq(
        Pos(cur._1.x + 1, cur._1.y)(grid.dim),
        Pos(cur._1.x - 1, cur._1.y)(grid.dim),
        Pos(cur._1.x, cur._1.y + 1)(grid.dim),
        Pos(cur._1.x, cur._1.y - 1)(grid.dim)
      ).filter(grid.dim.isInRange)
        .filter(p => grid(p) - grid(cur._1) <= 1)
        .filter(p => !visited.contains(p))
      val _ = q.enqueueAll(candidates.map(c => (c, cur._2 + 1)))
      visited.addAll(candidates)
    }
    cur._2
  }
}
