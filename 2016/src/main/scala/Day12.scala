import com.yannmoisan.util.fp.loop
import monocle.function.Index.index
import monocle.macros.Lenses

object Day12 extends MultiPuzzle[Int, Int] {

  val inc = """inc (\w)""".r
  val dec = """dec (\w)""".r
  val jnz = """jnz (\w) (-?\d+)""".r
  val cpyInt = """cpy (\d+) (\w)""".r
  val cpyReg = """cpy (\w) (\w)""".r

  trait Instruction

  case class Inc(r: Register) extends Instruction

  case class Dec(r: Register) extends Instruction

  case class Jnz(r: Register, v: Int) extends Instruction

  case class CpyInt(v: Int, to: Register) extends Instruction

  case class CpyReg(from: Register, to: Register) extends Instruction

  def parse: String => Instruction = {
    case inc(r) => Inc(r)
    case dec(r) => Dec(r)
    case jnz(r, v) => Jnz(r, v.toInt)
    case cpyInt(v, to) => CpyInt(v.toInt, to)
    case cpyReg(from, to) => CpyReg(from, to)
  }

  type Register = String
  type Mem = Map[String, Int]

  @Lenses
  case class State(mem: Mem, pointer: Int)

  def processInstruction(instructions: Seq[Instruction]): State => State = { s =>
    def reg(r: String) = State.mem composeOptional index(r)
    val ptr = instructions(s.pointer) match {
      case Jnz(r, v) if !(r != "1" && s.mem(r) == 0) => State.pointer.modify(_ + v)
      case _ => State.pointer.modify(_ + 1)
    }

    val mem = instructions(s.pointer) match {
      case Inc(r) => reg(r).modify(_ + 1)
      case Dec(r) => reg(r).modify(_ - 1)
      case CpyInt(v, to) => reg(to).set(v)
      case CpyReg(from, to) => reg(to).set(s.mem(from))
      case _ => s: State => s
    }
    (ptr andThen mem) (s)
  }

  def part(mem: Map[String, Int]) = { lines: Seq[String] =>
    val instructions = lines.map(parse)
    val init = State(mem, 0)
    val fs = loop(init)(processInstruction(instructions), _.pointer == instructions.size)
    fs.mem("a")
  }

  override def part1(lines: Iterator[String]) = part(Map[String, Int]("a" -> 0, "b" -> 0, "d" -> 0, "c" -> 0).withDefaultValue(0))(lines.toList)

  override def part2(lines: Iterator[String]) = part(Map[String, Int]("a" -> 0, "b" -> 0, "d" -> 0, "c" -> 1).withDefaultValue(0))(lines.toList)
}
