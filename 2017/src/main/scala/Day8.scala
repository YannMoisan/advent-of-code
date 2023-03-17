object Day8 extends MultiPuzzle[Int, Int] {
  final case class Instr(reg: String, op: String, amount: Int, cond: Condition)
  final case class Condition(reg: String, comp: String, value: Int)
  val instruct = """(\w+) (\w+) (-?\d+) if (\w+) (.+) (-?\d+)""".r

  final case class State(registers: Map[String, Int])

  def execute(instr: Instr, state: State): State = {
    val lhs = state.registers.getOrElse(instr.cond.reg, 0)
    val predicate: (Int, Int) => Boolean = instr.cond.comp match {
      case "==" => _ == _
      case "<"  => _ < _
      case "<=" => _ <= _
      case ">"  => _ > _
      case ">=" => _ >= _
      case "!=" => _ != _
    }
    val isCond = predicate(lhs, instr.cond.value)
    if (isCond)
      state.copy(
        registers = state.registers.updated(
          instr.reg,
          state.registers.getOrElse(instr.reg, 0) + (
            if (instr.op == "inc")
              instr.amount
            else
              -instr.amount
          )
        )
      )
    else
      state
  }

  def parse(line: String): Instr =
    line match {
      case instruct(a, b, c, d, e, f) =>
        Instr(a, b, Integer.parseInt(c), Condition(d, e, Integer.parseInt(f)))
    }

  override def part1(lines: Iterator[String]): Int = {
    val instructions = lines.map(parse)
    val state0       = State(Map.empty[String, Int])
    val allStates = instructions.scanLeft(state0) {
      case (s, instr) => execute(instr, s)
    }
    allStates.toSeq.lastOption.map(_.registers.maxBy(_._2)._2).getOrElse(0)

  }

  override def part2(lines: Iterator[String]): Int = {
    val instructions = lines.map(parse)
    val state0       = State(Map.empty[String, Int])
    val allStates = instructions.scanLeft(state0) {
      case (s, instr) => execute(instr, s)
    }
    allStates.flatMap(_.registers.values).max

  }
}
