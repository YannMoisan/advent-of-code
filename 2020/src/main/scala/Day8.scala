import scala.collection.mutable

object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val program: Array[Instruction] = input.map(Parser.parse).toArray
    run(program)._1
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
    var i   = 0
    var ret = (0, false)
    while (!ret._2) {
      ret = run(programs(i))
      i += 1
    }
    ret._1
  }

  private def run(program: Array[Instruction]): (Int, Boolean) = {
    var acc     = 0
    var ptr     = 0
    val visited = mutable.Set[Int]()
    while (!visited.contains(ptr) && ptr != program.length) {
      val _    = visited.add(ptr)
      val next = program(ptr)
      next match {
        case Nop(_) => ptr += 1
        case Acc(v) =>
          acc += v
          ptr += 1
        case Jmp(v) =>
          ptr += v
      }
    }
    (acc, ptr == program.length)
  }
}

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
