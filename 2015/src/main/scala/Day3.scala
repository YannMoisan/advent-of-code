import scala.collection.mutable

object Day3 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    var i       = 0
    var pos     = (0, 0)
    val visited = mutable.Set[(Int, Int)](pos)
    while (i < input.length) {
      input.charAt(i) match {
        case '^' => pos = (pos._1, pos._2 - 1)
        case 'v' => pos = (pos._1, pos._2 + 1)
        case '<' => pos = (pos._1 - 1, pos._2)
        case '>' => pos = (pos._1 + 1, pos._2)
      }
      val _ = visited.add(pos)
      i += 1
    }
    visited.size
  }

  override def part2(input: String): Int = {
    var i       = 0
    var pos1    = (0, 0)
    var pos2    = (0, 0)
    val visited = mutable.Set[(Int, Int)](pos1)
    while (i < input.length) {

      if (i % 2 == 0) {
        pos1 = input.charAt(i) match {
          case '^' => (pos1._1, pos1._2 - 1)
          case 'v' => (pos1._1, pos1._2 + 1)
          case '<' => (pos1._1 - 1, pos1._2)
          case '>' => (pos1._1 + 1, pos1._2)
        }
        val _ = visited.add(pos1)
      } else {
        pos2 = input.charAt(i) match {
          case '^' => (pos2._1, pos2._2 - 1)
          case 'v' => (pos2._1, pos2._2 + 1)
          case '<' => (pos2._1 - 1, pos2._2)
          case '>' => (pos2._1 + 1, pos2._2)
        }
        val _ = visited.add(pos2)
      }
      i += 1
    }
    visited.size
  }
}
