import com.yannmoisan.util.fp.loop

object Day25 extends MultiPuzzle[Int, Int] {

  val inc =
    """inc (\w)""".r
  val dec = """dec (\w)""".r
  val jnz = """jnz (\w) (-?\d+)""".r
  val jnzReg = """jnz (-?\d+) (\w)""".r
  val cpyInt = """cpy (-?\d+) (\w)""".r
  val cpyReg = """cpy (\w) (\w)""".r
  val tgl = """tgl (\w)""".r
  val out = """out b""".r


  type Register = String
  type Mem = Map[String, Int]

  // State monad
  case class State(mem: Mem, instructions: Seq[Instruction], pointer: Int, outputs: List[Int])

  trait Instruction

  case class Inc(r: Register) extends Instruction

  case class Dec(r: Register) extends Instruction

  case class Jnz(r: Register, v: Int) extends Instruction

  case class JnzReg(v: Int, r: Register) extends Instruction

  case class CpyInt(v: Int, to: Register) extends Instruction

  case class CpyReg(from: Register, to: Register) extends Instruction

  case class Tgl(r: Register) extends Instruction

  case object NoOp extends Instruction

  case object Out extends Instruction

  def parse: String => Instruction = {
    case inc(r) => Inc(r)
    case dec(r) => Dec(r)
    case jnz(r, v) => Jnz(r, v.toInt)
    case jnzReg(v, r) => JnzReg(v.toInt, r)
    case cpyInt(v, to) => CpyInt(v.toInt, to)
    case cpyReg(from, to) => CpyReg(from, to)
    case tgl(a) => Tgl(a)
    case out => Out
  }

  def toggle(i: Instruction): Instruction = i match {
    case Inc(r) => Dec(r)
    case Dec(r) => Inc(r)
    case Jnz(r, v) => NoOp
    case JnzReg(v, r) => CpyInt(v, r)
    case CpyInt(v, r) => JnzReg(v, r)
    case CpyReg(r1, r2) => NoOp
  }

  def processInstruction(s: State): State = {
    s.instructions(s.pointer) match {
      case Inc(r) => s.copy(
        mem = s.mem.updated(r, s.mem(r) + 1),
        pointer = s.pointer + 1
      )
      case Dec(r) => s.copy(
        mem = s.mem.updated(r, s.mem(r) - 1),
        pointer = s.pointer + 1
      )
      case CpyInt(v, to) => s.copy(
        mem = s.mem.updated(to, v),
        pointer = s.pointer + 1
      )
      case CpyReg(from, to) => s.copy(
        mem = s.mem.updated(to, s.mem(from)),
        pointer = s.pointer + 1
      )
      case Jnz(r, v) => if (r != "1" && s.mem(r) == 0)
        s.copy(pointer = s.pointer + 1)
      else
        s.copy(pointer = s.pointer + v)
      case JnzReg(v, r) => if (v.toInt == 0)
        s.copy(pointer = s.pointer + 1)
      else
        s.copy(pointer = s.pointer + s.mem(r))
      case Tgl(r) =>
        val ptr = s.pointer + s.mem(r)

        s.copy(
          instructions = if (ptr < s.instructions.length) s.instructions.updated(ptr, toggle(s.instructions(ptr))) else s.instructions,
          pointer = s.pointer + 1
        )
      case Out =>
        s.copy(
          outputs = s.outputs :+ s.mem("b"),
          pointer = s.pointer + 1
        )
    }
  }

  def part(instructions: Seq[Instruction], mem: Map[String, Int]) = {
    val init = State(mem, instructions, 0, List[Int]())
    val fs = loop(init)(processInstruction, s => s.pointer == s.instructions.size || s.outputs.size == 10)
    fs.outputs.mkString("")
  }

  override def part1(lines: Iterator[String]): Int = {
    val instructions = lines.map(parse).toList
    val stream = Stream.from(0).map(i => (i, part(instructions, Map[String, Int]("a" -> i).withDefaultValue(0))))
    val o = stream.find(t => (t._2.startsWith("0101010101") || t._2.startsWith("1010101010")))
    o.get._1
  }

  override def part2(lines: Iterator[String]) = 42
}
