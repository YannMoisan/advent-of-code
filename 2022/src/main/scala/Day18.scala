import scala.collection.mutable

object Day18 extends MultiPuzzle[Int, Int] {
  case class Position3(x: Int, y: Int, z: Int)
  object Position3 {
    def neighbors(p: Position3): Seq[Position3] =
      Seq(
        Position3(p.x + 1, p.y, p.z),
        Position3(p.x - 1, p.y, p.z),
        Position3(p.x, p.y + 1, p.z),
        Position3(p.x, p.y - 1, p.z),
        Position3(p.x, p.y, p.z + 1),
        Position3(p.x, p.y, p.z - 1)
      )
  }
  override def part1(input: Iterator[String]): Int = {
    val positions = input.map { case s"$x,$y,$z" => Position3(x.toInt, y.toInt, z.toInt) }.toSet
    println(positions.size)
    positions.toList.map(p => Position3.neighbors(p).count(n => !positions.contains(n))).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val positions = input.map { case s"$x,$y,$z" => Position3(x.toInt, y.toInt, z.toInt) }.toSet

    val queue = mutable.Queue[Position3]()
    queue.enqueue(Position3(0, 0, 0))
    val visited = mutable.Set[Position3]()

    while (!queue.isEmpty) {
      val current = queue.dequeue()
      Position3
        .neighbors(current)
        .filter(p => p.x >= -1 && p.x <= 22 && p.y >= -1 && p.y <= 22 && p.z >= -1 && p.z <= 22)
        .filter(p => !positions.contains(p))
        .filter(p => !visited.contains(p))
        .foreach { p =>
          queue.enqueue(p)
          visited.add(p)
        }
    }

    positions.toList
      .map(p => Position3.neighbors(p).count(n => !positions.contains(n) && visited.contains(n))).sum
  }
}
