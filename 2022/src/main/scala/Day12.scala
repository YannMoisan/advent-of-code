import scala.collection.mutable

@SuppressWarnings(
  Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial")
)
object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = new Grid(input.toArray.map(_.toArray))
    val s    = grid.posOf('S').get
    val e    = grid.posOf('E').get
    length(grid, s, e)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = new Grid(input.toArray.map(_.toArray))
    val e    = grid.posOf('E').get
    (0 to 40).map(y => Pos(0, y)).map(ss => length(grid, ss, e)).min
  }

  private def length(grid: Grid, s: Pos, e: Pos): Int = {
    grid(e) = 'z' // hacky
    val q       = mutable.Queue[(Pos, Int)]()
    val visited = mutable.Set[Pos]()
    val _       = q.enqueue((s, 0))
    var cur     = (s, 0)
    while (cur._1 != e) {
      cur = q.dequeue()
      val _ = visited += cur._1
      val candidates = Seq(
        Pos(cur._1.x + 1, cur._1.y),
        Pos(cur._1.x - 1, cur._1.y),
        Pos(cur._1.x, cur._1.y + 1),
        Pos(cur._1.x, cur._1.y - 1)
      ).filter(grid.isInRange)
        .filter(p => cur._1 == s || grid(p) - grid(cur._1) <= 1)
        .filter(p => !visited.contains(p))
      val _ = q.enqueueAll(candidates.map(c => (c, cur._2 + 1)))
      visited.addAll(candidates)
    }
    cur._2
  }
}
