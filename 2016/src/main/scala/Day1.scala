import cats.data.State
import cats.instances.all._
import cats.syntax.traverse._
import com.yannmoisan.util.collection.{next, prev}
import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.Direction

object Day1 extends SinglePuzzle[Int, Int] {

  case class PositionAndDir(pos: Position, dir: Direction)

  trait Instruction
  case object TurnLeft  extends Instruction
  case object TurnRight extends Instruction
  case object Walk      extends Instruction

  def parse(lines: String): Seq[Instruction] = {
    def toInstructions(s: String): Seq[Instruction] = s match {
      case "L" => Seq(TurnLeft)
      case "R" => Seq(TurnRight)
      case s   => List.fill(s.toString.toInt)(Walk)
    }
    lines
      .split(", ")
      .flatMap(s => Seq(s.take(1), s.substring(1)))
      .flatMap(toInstructions)
  }

  def update(i: Instruction): PositionAndDir => PositionAndDir =
    pad =>
      i match {
        case TurnLeft  => pad.copy(dir = prev(pad.dir, Direction.all4))
        case TurnRight => pad.copy(dir = next(pad.dir, Direction.all4))
        case Walk      => pad.copy(pos = move(pad))
      }

  def update2(i: Instruction): State[PositionAndDir, Option[Position]] =
    State.modify(update(i)).inspect(pad => if (i == Walk) Some(pad.pos) else None)

  def move(pad: PositionAndDir): Position =
    Position.move(pad.pos, pad.dir)

  override def part1(lines: String): Int = {
    val instructions = parse(lines)
    val state: State[PositionAndDir, List[Unit]] =
      instructions.map(i => State.modify(update(i))).toList.sequence
    val finalState = state.runS(PositionAndDir(Position(0, 0), Direction.Up)).value
    math.abs(finalState.pos.x) + math.abs(finalState.pos.y)
  }

  override def part2(lines: String): Int = {
    val instructions = parse(lines).toStream
    val state        = instructions.map(update2).sequence
    val positions    = state.runA(PositionAndDir(Position(0, 0), Direction.Up)).value.flatten
    val p            = firstDuplicate(positions).get
    math.abs(p.x) + math.abs(p.y)
  }

  def firstDuplicate[T](s: Stream[T], seen: Set[T] = Set.empty[T]): Option[T] = s match {
    case head #:: tail if seen(head) => Some(head)
    case head #:: tail               => firstDuplicate(tail, seen + head)
    case _                           => None
  }
}
