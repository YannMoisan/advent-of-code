import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

import scala.collection.mutable

object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val g       = Grid1D(input)
    val visited = mutable.Set[Int]()
    var sum     = 0
    while (visited.size != g.dim.indices.length) {
      val start     = g.dim.indices.toSet.diff(visited.toSet).head //toBeVisited.head
      var area      = 0
      var perimeter = 0

      // -- FLOOD FILL
      val q = mutable.Queue[Int]()
      //val visited = mutable.Set[Int]()
      val _ = q.enqueue(start)
      val _ = visited.add(start)
      while (!q.isEmpty) {
        val i = q.dequeue()
        area += 1
        perimeter += (4 - g.dim
          .neighbors4(i).count(k => g(k) == g(start))) // perimeter outside the map should be considered
        g.dim.neighbors4(i).foreach { j =>
          if (g(j) == g(start) && !visited.contains(j)) {
            // 1320076 (too low)
            val _ = q.enqueue(j)
            val _ = visited.add(j)
          }
        }
      }
      //println(s"[${g(start)}] area=$area, perimeter=$perimeter")
      sum += area * perimeter
      area = 0
      perimeter = 0
    }
    sum
  }

  override def part2(input: Iterator[String]): Int = {
    val g       = Grid1D(input)
    val visited = mutable.Set[Int]()
    var sum     = 0
    while (visited.size != g.dim.indices.length) {
      val start       = g.dim.indices.toSet.diff(visited.toSet).head //toBeVisited.head
      var area        = 0
      var perimeter   = 0
      var innerCorner = 0
      var outerCorner = 0

      // -- FLOOD FILL
      val q = mutable.Queue[Int]()
      //val visited = mutable.Set[Int]()
      val _ = q.enqueue(start)
      val _ = visited.add(start)
      while (!q.isEmpty) {
        val i = q.dequeue()
        area += 1
        perimeter += (4 - g.dim
          .neighbors4(i).count(k => g(k) == g(start))) // perimeter outside the map should be considered
        val p = g.dim.pos(i)

        // D1A
        // 4X2
        // C3B
        // Inner corners
        innerCorner += Seq(
          List(Pos(p.x, p.y - 1), Pos(p.x + 1, p.y)), // !1 & !2
          List(Pos(p.x + 1, p.y), Pos(p.x, p.y + 1)), // !2 & !3
          List(Pos(p.x, p.y + 1), Pos(p.x - 1, p.y)), // !3 & !4
          List(Pos(p.x - 1, p.y), Pos(p.x, p.y - 1))  // !4 & !1
        ).count(l => !eq(g, g(start), l(0)) && !eq(g, g(start), l(1)))
        // Outer corners
        outerCorner += Seq(
          List(Pos(p.x, p.y - 1), Pos(p.x + 1, p.y), Pos(p.x + 1, p.y - 1)), // 1 & 2 & !A
          List(Pos(p.x + 1, p.y), Pos(p.x, p.y + 1), Pos(p.x + 1, p.y + 1)), // 2 & 3 & !B
          List(Pos(p.x, p.y + 1), Pos(p.x - 1, p.y), Pos(p.x - 1, p.y + 1)), // 3 & 4 & !C
          List(Pos(p.x - 1, p.y), Pos(p.x, p.y - 1), Pos(p.x - 1, p.y - 1))  // 4 & 1 & !D
        ).count(l => eq(g, g(start), l(0)) && eq(g, g(start), l(1)) && !eq(g, g(start), l(2)))

        def eq[A](g: Grid[A], a: A, p: Pos): Boolean =
          if (p.x < 0 || p.x >= g.dim.width || p.y < 0 || p.y >= g.dim.height) false
          else g(p) == a

        g.dim.neighbors4(i).foreach { j =>
          if (g(j) == g(start) && !visited.contains(j)) {
            // 1320076 (too low)
            val _ = q.enqueue(j)
            val _ = visited.add(j)
          }
        }
      }
      //println(s"[${g(start)}] area=$area, perimeter=$perimeter")
      sum += area * (innerCorner + outerCorner) // perimeter
      area = 0
      perimeter = 0
    }
    sum
  }
}
