import com.yannmoisan.util.collection.{next, prev}
import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.Direction

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
      case TurnLeft  => pad.copy(dir = prev(pad.dir, Direction.all4))
      case TurnRight => pad.copy(dir = next(pad.dir, Direction.all4))
      case Walk      => pad.copy(pos = Position.move(pad.pos, pad.dir))
    }

  override def part1(lines: String): Int = {
    val instructions = parse(lines)
    val start        = PositionAndDir(Position(0, 0), Direction.Up)
    val finalState   = instructions.foldLeft(start)(update)
    Position.manhattan(finalState.pos, Position(0, 0))
  }

  override def part2(lines: String): Int = {
    val instructions = parse(lines)
    val start        = PositionAndDir(Position(0, 0), Direction.Up)
    val positions    = instructions.scanLeft(start)(update).map(_.pos).toVector
    val cleanedPositions = positions.zipWithIndex.collect {
      case (p, i) if i == positions.size - 1 || positions(i) != positions(i + 1) => p
    }
    val p = firstDuplicate(cleanedPositions.to(LazyList)).get
    Position.manhattan(p, Position(0, 0))
  }

  def firstDuplicate[T](s: LazyList[T], seen: Set[T] = Set.empty[T]): Option[T] = s match {
    case head #:: _ if seen(head) => Some(head)
    case head #:: tail            => firstDuplicate(tail, seen + head)
    case _                        => None
  }
}
