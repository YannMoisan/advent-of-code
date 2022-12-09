import scala.collection.mutable

object Day9 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = common(input, 2)
  override def part2(input: Iterator[String]): Int = common(input, 10)

  private def common(input: Iterator[String], size: Int): Int = {
    val nodes = Array.fill(size)((0, 0))
    val visited = mutable.Set[(Int, Int)]()
    val _ = visited.add(nodes(size-1))
    input.foreach { s =>
      val s"$dir $step" = s
      val vec = dir match {
        case "U" => (-1, 0)
        case "D" => (1, 0)
        case "L" => (0, -1)
        case "R" => (0, 1)
      }
      (0 until step.toInt).foreach { _ =>
        nodes(0) = (nodes(0)._1 + vec._1, nodes(0)._2 + vec._2)
        (1 to size - 1).foreach { i =>
          nodes(i) = next(nodes(i - 1), nodes(i))
          val _ = visited.add(nodes(size - 1))
        }
      }
    }
    visited.size
  }

  private def next(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    // the tail must move ?
    val diff = (head._1 - tail._1, head._2 - tail._2)
    if (math.abs(diff._1) > 1 || math.abs(diff._2) > 1) {
      (
        tail._1 + (if (diff._1 == 0) 0 else if (diff._1 > 0) 1 else -1),
        tail._2 + (if (diff._2 == 0) 0 else if (diff._2 > 0) 1 else -1)
      )
    } else
      tail
  }
}
