import scala.collection.mutable

object Day21 extends MultiPuzzle[Long, Long] {
//  sealed trait Instruction
//  case class Value(out: String, v: Long)            extends Instruction
//  case class Add(out: String, a: String, b: String) extends Instruction
//  case class Sub(out: String, a: String, b: String) extends Instruction
//  case class Mul(out: String, a: String, b: String) extends Instruction
//  case class Div(out: String, a: String, b: String) extends Instruction

  sealed trait Expression
  case class Value(out: String, v: Long)                    extends Expression
  case class Add(out: String, a: Expression, b: Expression) extends Expression
  case class Sub(out: String, a: Expression, b: Expression) extends Expression
  case class Mul(out: String, a: Expression, b: Expression) extends Expression
  case class Div(out: String, a: Expression, b: Expression) extends Expression

  override def part1(input: Iterator[String]): Long = {
    val m = mutable.Map[String, String]()

    input.foreach {
      case line @ s"$o: $_" => m(o) = line
    }

    val root = buildExpressionTree(m, "root")
    compute(root)
  }

  def buildExpressionTree(is: mutable.Map[String, String], current: String): Expression =
    is(current) match {
      case s"$o: $a + $b" => Add(o, buildExpressionTree(is, a), buildExpressionTree(is, b))
      case s"$o: $a - $b" => Sub(o, buildExpressionTree(is, a), buildExpressionTree(is, b))
      case s"$o: $a * $b" => Mul(o, buildExpressionTree(is, a), buildExpressionTree(is, b))
      case s"$o: $a / $b" => Div(o, buildExpressionTree(is, a), buildExpressionTree(is, b))
      case s"$o: $v"      => Value(o, v.toLong) // must be the last case, otherwise it will always match

    }

  def compute(expr: Expression): Long =
    expr match {
      case Value(_, v)  => v
      case Add(_, a, b) => compute(a) + compute(b)
      case Sub(_, a, b) => compute(a) - compute(b)
      case Mul(_, a, b) => compute(a) * compute(b)
      case Div(_, a, b) => compute(a) / compute(b)
    }

  def containsHuman(expr: Expression): Boolean =
    expr match {
      case Value("humn", _) => true
      case Value(_, _)      => false
      case Add(_, a, b)     => containsHuman(a) || containsHuman(b)
      case Sub(_, a, b)     => containsHuman(a) || containsHuman(b)
      case Mul(_, a, b)     => containsHuman(a) || containsHuman(b)
      case Div(_, a, b)     => containsHuman(a) || containsHuman(b)
    }

  override def part2(input: Iterator[String]): Long = {
    val m = mutable.Map[String, String]()

    input.foreach {
      case line @ s"$o: $_" => m(o) = line
    }

    val root = buildExpressionTree(m, "root")
    root match {
      case Add(_, a, b) => solve(a, b)
      case _            => throw new IllegalStateException()
    }
  }
  /*
  case Add(x, y)      if containsHuman(x) => solve(x, Subtract(rhs, y))
  case Add(x, y)      if containsHuman(y) => solve(y, Subtract(rhs, x))
  case Subtract(x, y) if containsHuman(x) => solve(x, Add(rhs, y))
  case Subtract(x, y) if containsHuman(y) => solve(y, Subtract(x, rhs))
  case Divide(x, y)   if containsHuman(x) => solve(x, Multiply(rhs, y))
  case Divide(x, y)   if containsHuman(y) => solve(y, Divide(x, rhs))
  case Multiply(x, y) if containsHuman(x) => solve(x, Divide(rhs, y))
  case Multiply(x, y) if containsHuman(y) => solve(y, Divide(rhs, x))

   * */
  def solve(left: Expression, right: Expression): Long =
    //println(left)
    left match {
      case Add(_, a, b) if containsHuman(b) => solve(b, Sub("?", right, a))
      case Add(_, a, b) if containsHuman(a) => solve(a, Sub("?", right, b))
      case Sub(_, a, b) if containsHuman(b) => solve(b, Sub("?", a, right))
      case Sub(_, a, b) if containsHuman(a) => solve(a, Add("?", right, b))
      case Div(_, a, b) if containsHuman(b) => solve(b, Div("?", a, right))
      case Div(_, a, b) if containsHuman(a) => solve(a, Mul("?", right, b))
      case Mul(_, a, b) if containsHuman(b) => solve(b, Div("?", right, a))
      case Mul(_, a, b) if containsHuman(a) => solve(a, Div("?", right, b))
      case _                                => compute(right)
    }
}
