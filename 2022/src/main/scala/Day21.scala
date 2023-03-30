import scala.collection.mutable

object Day21 extends MultiPuzzle[Long, Int] {
  sealed trait Instruction
  case class Value(out: String, v: Long)            extends Instruction
  case class Add(out: String, a: String, b: String) extends Instruction
  case class Sub(out: String, a: String, b: String) extends Instruction
  case class Mul(out: String, a: String, b: String) extends Instruction
  case class Div(out: String, a: String, b: String) extends Instruction
  case class Eq(out: String, a: String, b: String)  extends Instruction

  override def part1(input: Iterator[String]): Long = {
    val m = mutable.Map[String, Instruction]()

    input.foreach {
      case s"$o: $a + $b" => m(o) = Add(o, a, b)
      case s"$o: $a - $b" => m(o) = Sub(o, a, b)
      case s"$o: $a * $b" => m(o) = Mul(o, a, b)
      case s"$o: $a / $b" => m(o) = Div(o, a, b)
      case s"$o: $v" =>
        m(o) = Value(o, v.toLong) // must be the last case, otherwise it will always match
    }
    compute(m, "root")
  }

  def compute(is: mutable.Map[String, Instruction], current: String): Long =
    is(current) match {
      case Value(_, v)  => v
      case Add(_, a, b) => compute(is, a) + compute(is, b)
      case Sub(_, a, b) => compute(is, a) - compute(is, b)
      case Mul(_, a, b) => compute(is, a) * compute(is, b)
      case Div(_, a, b) => compute(is, a) / compute(is, b)
      case Eq(_, a, b)  => if (compute(is, a) == compute(is, b)) 1L else 0L
    }

  override def part2(input: Iterator[String]): Int = {
    val m = mutable.Map[String, Instruction]()

    input.foreach {
      case s"$o: $a + $b" => m(o) = Add(o, a, b)
      case s"$o: $a - $b" => m(o) = Sub(o, a, b)
      case s"$o: $a * $b" => m(o) = Mul(o, a, b)
      case s"$o: $a / $b" => m(o) = Div(o, a, b)
      case s"$o: $v" =>
        m(o) = Value(o, v.toLong) // must be the last case, otherwise it will always match
    }

    m("root") = Eq("root", "fflg", "qwqj")
    (-1000000 to 1000000).find { humn =>
      m("humn") = Value("humn", humn.toLong)
      compute(m, "root") == 1L
    }.get
  }
}
