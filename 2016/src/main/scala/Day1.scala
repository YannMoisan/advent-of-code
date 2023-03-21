import com.yannmoisan.util.collection.{firstDuplicate, next, prev}
import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.{Direction, Direction4}

object Day1 extends SinglePuzzle[Int, Int] {

  case class PositionAndDir(pos: Position, dir: Direction)

  trait Instruction
  case object TurnLeft  extends Instruction
  case object TurnRight extends Instruction
  case object Walk      extends Instruction

  def parse(lines: String): Array[Instruction] = {
    def toInstructions(s: String): Seq[Instruction] = s match {
      case "L" => Seq(TurnLeft)
      case "R" => Seq(TurnRight)
      case s   => List.fill(s.toInt)(Walk)
    }
    lines
      .split(", ")
      .flatMap(s => Seq(s.take(1), s.substring(1)))
      .flatMap(toInstructions)
  }

  def update(pad: PositionAndDir, i: Instruction): PositionAndDir =
    i match {
      case TurnLeft  => pad.copy(dir = prev(pad.dir, Direction4.all))
      case TurnRight => pad.copy(dir = next(pad.dir, Direction4.all))
      case Walk      => pad.copy(pos = Position.move(pad.pos, pad.dir))
    }

  override def part1(lines: String): Int = {
    val instructions = parse(lines)
    val start        = PositionAndDir(Position(0, 0), Direction4.Up)
    val finalState   = instructions.foldLeft(start)(update)
    Position.manhattan(finalState.pos, Position(0, 0))
  }

  override def part2(lines: String): Int = {
    val instructions = parse(lines)
    val start        = PositionAndDir(Position(0, 0), Direction4.Up)
    val positions    = instructions.scanLeft(start)(update).map(_.pos).toVector
    val cleanedPositions = positions.zipWithIndex.collect {
      case (p, i) if i == positions.size - 1 || positions(i) != positions(i + 1) => p
    }
    val p = firstDuplicate(cleanedPositions.iterator).get
    Position.manhattan(p, Position(0, 0))
  }
}
