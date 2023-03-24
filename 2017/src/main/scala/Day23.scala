object Day23 extends MultiPuzzle[Int, Int] {
  sealed trait Instruction
  case class set(x: String, y: String) extends Instruction
  case class sub(x: String, y: String) extends Instruction
  case class mul(x: String, y: String) extends Instruction
  case class jnz(x: String, y: String) extends Instruction

  object Instruction {
    def execute(s: State, in: Instruction): State =
      in match {
        case set(x, y) => State(s.index + 1, s.registers.updated(x, s.value(y)), s.mul)
        case sub(x, y) =>
          State(s.index + 1, s.registers.updated(x, s.registers(x) - s.value(y)), s.mul)
        case mul(x, y) =>
          State(s.index + 1, s.registers.updated(x, s.registers(x) * s.value(y)), s.mul + 1)
        case jnz(x, y) =>
          if (s.value(x) != 0) State(s.index + s.value(y).toInt, s.registers, s.mul)
          else State(s.index + 1, s.registers, s.mul)
      }
  }

  case class State(index: Int, registers: Map[String, Long], mul: Int) {
    def value(valOrReg: String): Long =
      if (valOrReg.head.isLetter) registers(valOrReg) else valOrReg.toLong
  }

  override def part1(input: Iterator[String]): Int = {
    val instructions: Array[Instruction] = input.map {
      _ match {
        case s"set $x $y" => set(x, y)
        case s"sub $x $y" => sub(x, y)
        case s"mul $x $y" => mul(x, y)
        case s"jnz $x $y" => jnz(x, y)
      }
    }.toArray
    val init = State(0, Map("a" -> 0, "h" -> 0), 0)
    Iterator
      .iterate(init)(s => Instruction.execute(s, instructions(s.index)))
      .find(_.index >= instructions.size)
      .get.mul //map { x => println(x); x }.toList.last.mul
  }

  override def part2(input: Iterator[String]): Int = {
    val instructions: Array[Instruction] = input.map {
      _ match {
        case s"set $x $y" => set(x, y)
        case s"sub $x $y" => sub(x, y)
        case s"mul $x $y" => mul(x, y)
        case s"jnz $x $y" => jnz(x, y)
      }
    }.toArray
    val init = State(0, Map("a" -> 1, "h" -> 0), 0)
    Iterator
      .iterate(init)(s => Instruction.execute(s, instructions(s.index)))
      .map { x => println(x); x }
      .find(_.index >= instructions.size)
      .get.mul //map { x => println(x); x }.toList.last.mul
  }
}
