object Day18 extends MultiPuzzle[Long, Int] {

  override def part1(input: Iterator[String]): Long = {
    val instructions: Array[Instruction] = input.map {
      _ match {
        case s"snd $x"    => snd(x)
        case s"set $x $y" => set(x, y)
        case s"add $x $y" => add(x, y)
        case s"mul $x $y" => mul(x, y)
        case s"mod $x $y" => mod(x, y)
        case s"rcv $x"    => rcv(x)
        case s"jgz $x $y" => jgz(x, y)
      }
    }.toArray
    val init = State(0, Map("p" -> 0), 0, None)
    Iterator
      .iterate(init)(s => Instruction.execute(s, instructions(s.index)))
      .find(_.rcv.isDefined).get
      .rcv.get
  }

  override def part2(input: Iterator[String]): Int = 43
}

sealed trait Instruction
case class snd(x: String)            extends Instruction
case class set(x: String, y: String) extends Instruction
case class add(x: String, y: String) extends Instruction
case class mul(x: String, y: String) extends Instruction
case class mod(x: String, y: String) extends Instruction
case class rcv(x: String)            extends Instruction
case class jgz(x: String, y: String) extends Instruction

object Instruction {
  def execute(s: State, in: Instruction): State =
    in match {
      case snd(x)    => State(s.index + 1, s.registers, mem = s.registers(x), s.rcv)
      case set(x, y) => State(s.index + 1, s.registers.updated(x, s.value(y)), s.mem, s.rcv)
      case add(x, y) =>
        State(s.index + 1, s.registers.updated(x, s.registers(x) + s.value(y)), s.mem, s.rcv)
      case mul(x, y) =>
        State(s.index + 1, s.registers.updated(x, s.registers(x) * s.value(y)), s.mem, s.rcv)
      case mod(x, y) =>
        State(s.index + 1, s.registers.updated(x, s.registers(x) % s.value(y)), s.mem, s.rcv)
      case rcv(x) =>
        if (s.registers(x) != 0)
          State(s.index + 1, s.registers, s.mem, Some(s.mem))
        else
          State(s.index + 1, s.registers, s.mem, s.rcv)
      case jgz(x, y) =>
        if (s.value(x) > 0) State(s.index + s.value(y).toInt, s.registers, s.mem, s.rcv)
        else State(s.index + 1, s.registers, s.mem, s.rcv)
    }
}

case class State(index: Int, registers: Map[String, Long], mem: Long, rcv: Option[Long]) {
  def value(valOrReg: String): Long =
    if (valOrReg.head.isLetter) registers(valOrReg) else valOrReg.toLong
}
