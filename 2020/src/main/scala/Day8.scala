object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val program: Array[Instruction] = input.map(Parser.parse).toArray
    val initial                     = State(0, 0, Set[Int]())
    loop(initial)(executeNextInstruction(program), s => s.visited.contains(s.ptr)).acc
  }

  override def part2(input: Iterator[String]): Int = {
    val program: Array[Instruction] = input.map(Parser.parse).toArray

    val programs: Seq[Array[Instruction]] = (0 until program.length).flatMap { i =>
      program(i) match {
        case Acc(_) => None
        case Jmp(v) =>
          val p = program.clone()
          p(i) = Nop(v)
          Some(p)
        case Nop(v) =>
          val p = program.clone()
          p(i) = Jmp(v)
          Some(p)
      }
    }

    Iterator
      .from(0).map { i =>
        val initial = State(0, 0, Set[Int]())
        loop(initial)(
          executeNextInstruction(programs(i)),
          s => s.visited.contains(s.ptr) || s.ptr == program.length
        )
      }.find(_.ptr == program.length) match {
      case Some(state) => state.acc
      case _           => sys.error("illegal state")
    }
  }

  private def executeNextInstruction(program: Array[Instruction])(state: State): State = {
    val newVisited = state.visited.+(state.ptr)
    program(state.ptr) match {
      case Nop(_) => State(state.acc, state.ptr + 1, newVisited)
      case Acc(v) => State(state.acc + v, state.ptr + 1, newVisited)
      case Jmp(v) => State(state.acc, state.ptr + v, newVisited)
    }
  }

  def loop[A](s: A)(f: A => A, stop: A => Boolean): A = {
    val newS = f(s)
    if (stop(newS)) newS else loop(newS)(f, stop)
  }
}

final case class State(acc: Int, ptr: Int, visited: Set[Int])

object Parser {
  def parse(line: String): Instruction = line.split(" ") match {
    case Array("nop", v) => Nop(v.toInt)
    case Array("acc", v) => Acc(v.toInt)
    case Array("jmp", v) => Jmp(v.toInt)
  }
}

trait Instruction      extends Product with Serializable
case class Nop(v: Int) extends Instruction
case class Acc(v: Int) extends Instruction
case class Jmp(v: Int) extends Instruction
