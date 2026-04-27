import fastparse._, NoWhitespace._

object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    def number[X: P]: P[Int]      = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def operator[X: P]: P[String] = P("+").!
    def expr[X: P]                = P(parser | number)
    def parser[X: P]: P[Int] = P(expr ~ operator ~ number).map {
      case (lhs, "+", rhs) => lhs + rhs
    }

    println(fastparse.parse("3+4+5", parser(_)))
    43
  }

  override def part2(input: Iterator[String]): Int = 44
}
