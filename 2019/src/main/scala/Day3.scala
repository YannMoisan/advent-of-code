import scala.collection.Iterator

object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(lines: Iterator[String]) : Int = {
    val line1        = lines.next()
    val line2        = lines.next()
    val path1        = fullPath(parseLine(line1))
    val path2        = fullPath(parseLine(line2))
    val intersection = path1.intersect(path2)
    intersection.map { p =>
      math.abs(p.x) + math.abs(p.y)
    }.min
  }

  override def part2(lines: Iterator[String]) : Int = {
    val line1        = lines.next()
    val line2        = lines.next()
    val path1        = fullPath(parseLine(line1))
    val path2        = fullPath(parseLine(line2))
    val intersection = path1.intersect(path2)
    intersection.map { pos =>
      path1.indexOf(pos) + path2.indexOf(pos) + 2
    }.min
  }

  def parseLine(l: String): Array[Move] = l.split(",").map { s =>
    Move(s.head, s.tail.toInt)
  }

  def fullPath(path: Array[Move]): Vector[Pos] = {
    val init = State(Pos(0, 0), Vector.empty[Pos])

    val state = path.foldLeft(init) {
      case (state, move) =>
        val range = (1 to move.length)
        val newPath = move.direction match {
          case 'D' =>
            range.map(i => Pos(state.pos.x, state.pos.y - i))
          case 'U' =>
            range.map(i => Pos(state.pos.x, state.pos.y + i))
          case 'R' =>
            range.map(i => Pos(state.pos.x + i, state.pos.y))
          case 'L' =>
            range.map(i => Pos(state.pos.x - i, state.pos.y))
        }
        State(newPath.last, state.fullPath ++ newPath)
    }
    state.fullPath
  }
}

final case class Move(direction: Char, length: Int)
final case class Pos(x: Int, y: Int)
final case class State(pos: Pos, fullPath: Vector[Pos])
