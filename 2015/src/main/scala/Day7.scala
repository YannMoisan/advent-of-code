import scala.collection.mutable

object Day7 extends MultiPuzzle[Int, Int] {
  def parse(s: String): Instr = s match {
    case s"1 AND $b -> $o"     => And1(b, o)
    case s"$a AND $b -> $o"    => And(a, b, o)
    case s"$a OR $b -> $o"     => Or(a, b, o)
    case s"$i LSHIFT $q -> $o" => LeftShift(i, q.toInt, o)
    case s"$i RSHIFT $q -> $o" => RightShift(i, q.toInt, o)
    case s"NOT $i -> $o"       => Not(i, o)
    case s"$v -> $o"           => if (v.forall(_.isDigit)) Input(v.toInt, o) else Link(v, o)
  }

  override def part1(input: Iterator[String]): Int = {
    val map = mutable.Map[String, Int]()

    val instructions = input.map(parse).toList

    var i = 0
    while (!map.contains("a") && i < 500) {
      i = i + 1
      instructions.foreach { instr =>
        instr match {
          case Input(v, o) => map(o) = v
          case Link(i, o)  => if (map.contains(i)) map(o) = map(i)
          case Not(i, o)   => if (map.contains(i)) map(o) = (~map(i)).toChar.toInt
          case And1(b, c) =>
            if (map.contains(b)) map(c) = (1 & map(b)).toChar.toInt
          case And(a, b, c) =>
            if (map.contains(a) && map.contains(b)) map(c) = (map(a) & map(b)).toChar.toInt
          case Or(a, b, c) =>
            if (map.contains(a) && map.contains(b)) map(c) = (map(a) | map(b)).toChar.toInt
          case LeftShift(a, qty, o)  => if (map.contains(a)) map(o) = map(a) << qty
          case RightShift(a, qty, o) => if (map.contains(a)) map(o) = map(a) >> qty
        }
      }
    }
    map("a")
  }

  override def part2(input: Iterator[String]): Int = {
    val map = mutable.Map[String, Int]("b" -> 16076)

    val instructions = input.map(parse).toList.filter {
      case Input(_, "b") => false
      case _             => true
    }

    var i = 0
    while (!map.contains("a") && i < 500) {
      i = i + 1
      instructions.foreach { instr =>
        instr match {
          case Input(v, o) => map(o) = v
          case Link(i, o)  => if (map.contains(i)) map(o) = map(i)
          case Not(i, o)   => if (map.contains(i)) map(o) = (~map(i)).toChar.toInt
          case And1(b, c) =>
            if (map.contains(b)) map(c) = (1 & map(b)).toChar.toInt
          case And(a, b, c) =>
            if (map.contains(a) && map.contains(b)) map(c) = (map(a) & map(b)).toChar.toInt
          case Or(a, b, c) =>
            if (map.contains(a) && map.contains(b)) map(c) = (map(a) | map(b)).toChar.toInt
          case LeftShift(a, qty, o)  => if (map.contains(a)) map(o) = map(a) << qty
          case RightShift(a, qty, o) => if (map.contains(a)) map(o) = map(a) >> qty
        }
      }
    }
    map("a")
  }
}

sealed trait Instr
case class And1(input2: String, output: String)                extends Instr
case class And(input1: String, input2: String, output: String) extends Instr
case class Or(input1: String, input2: String, output: String)  extends Instr
case class RightShift(input: String, qty: Int, output: String) extends Instr
case class LeftShift(input: String, qty: Int, output: String)  extends Instr
case class Not(input: String, output: String)                  extends Instr
case class Input(value: Int, output: String)                   extends Instr
case class Link(input: String, output: String)                 extends Instr
