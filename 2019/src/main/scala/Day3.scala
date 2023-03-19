import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.Direction

object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(lines: Iterator[String]): Int = {
    val line1        = lines.next()
    val line2        = lines.next()
    val path1        = fullPath(parseLine(line1))
    val path2        = fullPath(parseLine(line2))
    val intersection = path1.intersect(path2)
    intersection.map(p => math.abs(p.x) + math.abs(p.y)).min
  }

  override def part2(lines: Iterator[String]): Int = {
    val line1        = lines.next()
    val line2        = lines.next()
    val path1        = fullPath(parseLine(line1))
    val path2        = fullPath(parseLine(line2))
    val intersection = path1.intersect(path2)
    intersection.map(pos => path1.indexOf(pos) + path2.indexOf(pos) + 2).min
  }

  def parseLine(l: String): Array[Move] = l.split(",").map { s =>
    val dir = s.head match {
      case 'D' => Direction.Down
      case 'U' => Direction.Up
      case 'R' => Direction.Right
      case 'L' => Direction.Left
    }
    Move(dir, s.tail.toInt)
  }

  def fullPath(path: Array[Move]): Vector[Position] = {
    val init = Vector(Position(0, 0))

    val positions = path.foldLeft(init) {
      case (state, move) =>
        val newPath =
          Iterator.iterate(state.last)(Position.move(_, move.direction)).drop(1).take(move.length)
        state ++ newPath
    }
    positions.drop(1) // remove Position(0,0)
  }
}

final case class Move(direction: Direction, length: Int)
