import com.yannmoisan.util.fp.loop

object Day23 extends MultiPuzzle[Int, Int] {

  val inc    = """inc (\w)""".r
  val dec    = """dec (\w)""".r
  val jnz    = """jnz (\w) (-?\d+)""".r
  val jnzReg = """jnz (-?\d+) (\w)""".r
  val cpyInt = """cpy (-?\d+) (\w)""".r
  val cpyReg = """cpy (\w) (\w)""".r
  val tgl    = """tgl (\w)""".r

  type Register = String
  type Mem      = Map[String, Int]

  case class State(mem: Mem, instructions: Seq[Instruction], pointer: Int)

  trait Instruction

  case class Inc(r: Register) extends Instruction

  case class Dec(r: Register) extends Instruction

  case class Jnz(r: Register, v: Int) extends Instruction

  case class JnzReg(v: Int, r: Register) extends Instruction

  case class CpyInt(v: Int, to: Register) extends Instruction

  case class CpyReg(from: Register, to: Register) extends Instruction

  case class Tgl(r: Register) extends Instruction

  case object NoOp extends Instruction

  def parse: String => Instruction = {
    case inc(r)           => Inc(r)
    case dec(r)           => Dec(r)
    case jnz(r, v)        => Jnz(r, v.toInt)
    case jnzReg(v, r)     => JnzReg(v.toInt, r)
    case cpyInt(v, to)    => CpyInt(v.toInt, to)
    case cpyReg(from, to) => CpyReg(from, to)
    case tgl(a)           => Tgl(a)
  }

  def toggle(i: Instruction): Instruction = i match {
    case Inc(r)       => Dec(r)
    case Dec(r)       => Inc(r)
    case Jnz(_, _)    => NoOp
    case JnzReg(v, r) => CpyInt(v, r)
    case CpyInt(v, r) => JnzReg(v, r)
    case CpyReg(_, _) => NoOp
  }

  def processInstruction(s: State): State =
    s.instructions(s.pointer) match {
      case Inc(r) =>
        s.copy(
          mem = s.mem.updated(r, s.mem(r) + 1),
          pointer = s.pointer + 1
        )
      case Dec(r) =>
        s.copy(
          mem = s.mem.updated(r, s.mem(r) - 1),
          pointer = s.pointer + 1
        )
      case CpyInt(v, to) =>
        s.copy(
          mem = s.mem.updated(to, v),
          pointer = s.pointer + 1
        )
      case CpyReg(from, to) =>
        s.copy(
          mem = s.mem.updated(to, s.mem(from)),
          pointer = s.pointer + 1
        )
      case Jnz(r, v) =>
        if (r != "1" && s.mem(r) == 0)
          s.copy(pointer = s.pointer + 1)
        else
          s.copy(pointer = s.pointer + v)
      case JnzReg(v, r) =>
        if (v.toInt == 0)
          s.copy(pointer = s.pointer + 1)
        else
          s.copy(pointer = s.pointer + s.mem(r))
      case Tgl(r) =>
        val ptr = s.pointer + s.mem(r)

        s.copy(
          instructions =
            if (ptr < s.instructions.length)
              s.instructions.updated(ptr, toggle(s.instructions(ptr)))
            else s.instructions,
          pointer = s.pointer + 1
        )
    }

  def part(mem: Map[String, Int]) = { lines: Seq[String] =>
    val instructions = lines.map(parse)
    val init         = State(mem, instructions, 0)
    val fs           = loop(init)(processInstruction, s => s.pointer == s.instructions.size)
    fs.mem("a")
  }

  override def part1(lines: Iterator[String]): Int =
    part(Map[String, Int]("a" -> 7).withDefaultValue(0))(lines.toList)

  override def part2(lines: Iterator[String]): Int =
    part(Map[String, Int]("a" -> 12).withDefaultValue(0))(lines.toList)
}
