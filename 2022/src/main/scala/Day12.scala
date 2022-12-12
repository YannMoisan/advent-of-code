import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Array[Array[Char]] = input.toArray.map(_.toArray)
    val s = for {
      r <- grid.indices
      c <- grid.head.indices
      if grid(r)(c) == 'S'
    } yield (r,c)
    val e = for {
      r <- grid.indices
      c <- grid.head.indices
      if grid(r)(c) == 'E'
    } yield (r, c)
    length(grid, s.head, e.head)
  }


  override def part2(input: Iterator[String]): Int = {
    val grid: Array[Array[Char]] = input.toArray.map(_.toArray)
    val e = for {
      r <- grid.indices
      c <- grid.head.indices
      if grid(r)(c) == 'E'
    } yield (r, c)
    (0 to 40).map(i => (i, 0)).map(ss => length(grid, ss, e.head)).min
  }

  private def length(grid: Array[Array[Char]], s: (Int, Int), e: (Int, Int)):Int = {
    grid(e._1)(e._2) = 'z' // hacky
    val q = mutable.Queue[((Int, Int), Int)]()
    val visited = mutable.Set[(Int, Int)]()
    val _ = q.enqueue((s, 0))
    //visited += s
    var cur = (s,0)
    while (cur._1 != e) {
      cur = q.dequeue()
      val _  = visited += cur._1
      val candidates = Seq(
        (cur._1._1 + 1, cur._1._2),
        (cur._1._1 - 1, cur._1._2),
        (cur._1._1, cur._1._2 + 1),
        (cur._1._1, cur._1._2 - 1),
      ).filter {
        case (r, c) => r >= 0 && r < grid.length && c >= 0 && c < grid.head.length
      }.filter {
        case (r, c) => cur._1 == s || grid(r)(c) - grid(cur._1._1)(cur._1._2) <= 1
      }.filter {
        p => !visited.contains(p)
      }
      val _ = q.enqueueAll(candidates.map(c => (c, cur._2 + 1)))
      visited.addAll(candidates)
    }
    cur._2
  }
}
