object Day12 extends MultiPuzzle[Int, Int] {
  case class State(pos: Position, dir: Direction)

  case class Direction(dx: Int, dy: Int) {
    def *(n: Int): Direction           = Direction(n * dx, n * dy)
    def +(other: Direction): Direction = Direction(other.dx + dx, other.dy + dy)
    //https://en.wikipedia.org/wiki/Rotation_matrix
    def rotate(angleInDegrees: Int): Direction = {
      val angle = math.toRadians(angleInDegrees.toDouble)
      Direction(
        dx * math.cos(angle).toInt - dy * math.sin(angle).toInt,
        dx * math.sin(angle).toInt + dy * math.cos(angle).toInt
      )
    }
  }
  object Direction {
    val north: Direction = Direction(0, -1)
    val south: Direction = Direction(0, 1)
    val east: Direction  = Direction(1, 0)
    val west: Direction  = Direction(-1, 0)
  }

  case class Position(x: Int, y: Int) {
    def +(dir: Direction): Position = Position(x + dir.dx, y + dir.dy)
  }

  override def part1(input: Iterator[String]): Int = {
    val end = input.foldLeft(State(Position(0, 0), Direction.east)) { (acc, line) =>
      val value = line.tail.toInt
      line.head match {
        case 'N' => acc.copy(pos = acc.pos + (Direction.north * value))
        case 'S' => acc.copy(pos = acc.pos + (Direction.south * value))
        case 'E' => acc.copy(pos = acc.pos + (Direction.east * value))
        case 'W' => acc.copy(pos = acc.pos + (Direction.west * value))
        case 'L' => acc.copy(dir = acc.dir.rotate(360 - value))
        case 'R' => acc.copy(dir = acc.dir.rotate(value))
        case 'F' => acc.copy(pos = acc.pos + (acc.dir * value))
      }
    }
    math.abs(end.pos.x) + math.abs(end.pos.y)
  }

  override def part2(input: Iterator[String]): Int = {
    val end = input.foldLeft(State(Position(0, 0), Direction(10, -1))) { (acc, line) =>
      val value = line.tail.toInt
      line.head match {
        case 'N' => acc.copy(dir = acc.dir + (Direction.north * value))
        case 'S' => acc.copy(dir = acc.dir + (Direction.south * value))
        case 'E' => acc.copy(dir = acc.dir + (Direction.east * value))
        case 'W' => acc.copy(dir = acc.dir + (Direction.west * value))
        case 'L' => acc.copy(dir = acc.dir.rotate(360 - value))
        case 'R' => acc.copy(dir = acc.dir.rotate(value))
        case 'F' => acc.copy(pos = acc.pos + (acc.dir * value))
      }
    }
    math.abs(end.pos.x) + math.abs(end.pos.y)
  }
}
