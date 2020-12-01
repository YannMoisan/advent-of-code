import scala.collection.mutable

object Day7 extends MultiPuzzle[Int, Int] {
  def parse(s: String): Instr = s match {
    case s"$a AND $b -> $o"    => And(a, b, o)
    case s"$a OR $b -> $o"     => Or(a, b, o)
    case s"$i LSHIFT $q -> $o" => LeftShift(i, q.toInt, o)
    case s"$i RSHIFT $q -> $o" => RightShift(i, q.toInt, o)
    case s"NOT $i -> $o"       => Not(i, o)
    case s"$v -> $o"           => if (v.forall(_.isDigit)) Input(v.toInt, o) else Link(v, o)
  }

  override def part1(input: Iterator[String]): Int = {
    val map = mutable.Map[String, Int]()

    val ex = """123 -> x
               |456 -> y
               |x AND y -> d
               |x OR y -> e
               |x LSHIFT 2 -> f
               |y RSHIFT 2 -> g
               |NOT x -> h
               |NOT y -> i""".stripMargin

    val instructions = ex.split('\n').map(parse).toList

    var i = 0
    while (!map.contains("a") && i < 10) {
      i = i + 1
      instructions.foreach { instr =>
        instr match {
          case Input(v, o)           => map(o) = v
          case Link(i, o)            => if (map.contains(i)) map(o) = map(i)
          case Not(i, o)             => if (map.contains(i)) map(o) = ~map(i)
          case And(a, b, c)          => if (map.contains(a) && map.contains(b)) map(c) = map(a) & map(b)
          case Or(a, b, c)           => if (map.contains(a) && map.contains(b)) map(c) = map(a) | map(b)
          case LeftShift(a, qty, o)  => if (map.contains(a)) map(o) = map(a) << qty
          case RightShift(a, qty, o) => if (map.contains(a)) map(o) = map(a) >> qty
        }
      }
    }
    //println(map(a))

//    take(5).mkString(","))
    println(map)
    map("a")
  }

  override def part2(input: Iterator[String]): Int = 43
}

sealed trait Instr
case class And(input1: String, input2: String, output: String) extends Instr
case class Or(input1: String, input2: String, output: String)  extends Instr
case class RightShift(input: String, qty: Int, output: String) extends Instr
case class LeftShift(input: String, qty: Int, output: String)  extends Instr
case class Not(input: String, output: String)                  extends Instr
case class Input(value: Int, output: String)                   extends Instr
case class Link(input: String, output: String)                 extends Instr
