object Day23 extends MultiPuzzle[Int, Int] {
  trait Instruction                               extends Product with Serializable
  case class Half(reg: String)                    extends Instruction
  case class Triple(reg: String)                  extends Instruction
  case class Increment(reg: String)               extends Instruction
  case class Jump(offset: Int)                    extends Instruction
  case class JumpIfEven(reg: String, offset: Int) extends Instruction
  case class JumpIfOne(reg: String, offset: Int)  extends Instruction

  case class State(registers: Map[String, Int], position: Int)

  object State {
    val init1 = State(Map("a" -> 0, "b" -> 0), 0)
    val init2 = State(Map("a" -> 1, "b" -> 0), 0)
  }

  override def part1(input: Iterator[String]): Int = {
    val program = input.map(parseInstruction).toVector
    Iterator
      .iterate(State.init1)(s => applyInstruction(program(s.position), s)).find(
        _.position >= program.size
      ).get.registers("b")
  }

  override def part2(input: Iterator[String]): Int = {
    val program = input.map(parseInstruction).toVector
    Iterator
      .iterate(State.init2)(s => applyInstruction(program(s.position), s)).find(
        _.position >= program.size
      ).get.registers("b")
  }

  private def parseInstruction(line: String): Instruction =
    line match {
      case s"hlf $reg"          => Half(reg)
      case s"tpl $reg"          => Triple(reg)
      case s"inc $reg"          => Increment(reg)
      case s"jmp $offset"       => Jump(offset.toInt)
      case s"jie $reg, $offset" => JumpIfEven(reg, offset.toInt)
      case s"jio $reg, $offset" => JumpIfOne(reg, offset.toInt)
    }

  // 2 designs : methods on Instruction or a global one
  private def applyInstruction(i: Instruction, s: State): State =
    i match {
      case Half(reg)      => State(s.registers.+((reg, s.registers(reg) / 2)), s.position + 1)
      case Triple(reg)    => State(s.registers.+((reg, s.registers(reg) * 3)), s.position + 1)
      case Increment(reg) => State(s.registers.+((reg, s.registers(reg) + 1)), s.position + 1)
      case Jump(offset)   => State(s.registers, s.position + offset)
      case JumpIfEven(reg, offset) =>
        if (s.registers(reg) % 2 == 0)
          State(s.registers, s.position + offset)
        else
          State(s.registers, s.position + 1)
      case JumpIfOne(reg, offset) =>
        if (s.registers(reg) == 1)
          State(s.registers, s.position + offset)
        else
          State(s.registers, s.position + 1)
    }
}
