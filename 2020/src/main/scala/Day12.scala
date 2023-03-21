import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.{Direction, Direction4}

object Day12 extends MultiPuzzle[Int, Int] {
  case class State(pos: Position, dir: Direction)

  override def part1(input: Iterator[String]): Int = {
    val end = input.foldLeft(State(Position(0, 0), Direction4.Right)) { (acc, line) =>
      val value = line.tail.toInt
      line.head match {
        case 'N' => acc.copy(pos = Position.move(acc.pos, Direction4.Up * value))
        case 'S' => acc.copy(pos = Position.move(acc.pos, Direction4.Down * value))
        case 'E' => acc.copy(pos = Position.move(acc.pos, Direction4.Right * value))
        case 'W' => acc.copy(pos = Position.move(acc.pos, Direction4.Left * value))
        case 'L' => acc.copy(dir = acc.dir.rotate(360 - value))
        case 'R' => acc.copy(dir = acc.dir.rotate(value))
        case 'F' => acc.copy(pos = Position.move(acc.pos, acc.dir * value))
      }
    }
    Position.manhattan(end.pos, Position(0, 0))
  }

  override def part2(input: Iterator[String]): Int = {
    val end = input.foldLeft(State(Position(0, 0), new Direction(10, -1, -1) {})) { (acc, line) =>
      val value = line.tail.toInt
      line.head match {
        case 'N' => acc.copy(dir = acc.dir + (Direction4.Up * value))
        case 'S' => acc.copy(dir = acc.dir + (Direction4.Down * value))
        case 'E' => acc.copy(dir = acc.dir + (Direction4.Right * value))
        case 'W' => acc.copy(dir = acc.dir + (Direction4.Left * value))
        case 'L' => acc.copy(dir = acc.dir.rotate(360 - value))
        case 'R' => acc.copy(dir = acc.dir.rotate(value))
        case 'F' => acc.copy(pos = Position.move(acc.pos, acc.dir * value))
      }
    }
    Position.manhattan(end.pos, Position(0, 0))
  }
}
