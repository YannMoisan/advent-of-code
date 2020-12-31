import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day24 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    //val first = input.next()

    val s = mutable.Set[(Int, Int, Int)]()

    input.foreach { line =>
      val parsedLine = parseLine(line)
      val pos        = move(parsedLine)
      if (s.contains(pos)) s.remove(pos) else s.add(pos)
    }
    s.size
  }

  def parseLine(first: String): List[(Int, Int, Int)] = {
    var i   = 0
    val buf = ListBuffer[(Int, Int, Int)]()
    while (i < first.length) {
      val vect = if (first(i) == 'w') {
        i += 1
        (-1, 1, 0)
      } else if (first(i) == 'e') {
        i += 1
        (1, -1, 0)
      } else {
        val tup = (first(i), first(i + 1))
        i += 2
        tup match {
          case ('s', 'e') => (0, -1, 1)
          case ('s', 'w') => (-1, 0, 1)
          case ('n', 'e') => (1, 0, -1)
          case ('n', 'w') => (0, 1, -1)
        }
      }
      buf.addOne(vect)
    }
    buf.toList
  }

  def move(moves: List[(Int, Int, Int)]): (Int, Int, Int) = moves.foldLeft((0, 0, 0)) { (a, b) =>
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  }

  override def part2(input: Iterator[String]): Int = ???
}
