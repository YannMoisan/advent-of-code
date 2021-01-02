import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day24 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
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

  override def part2(input: Iterator[String]): Int = {
    var blacks = mutable.Set[(Int, Int, Int)]()

    input.foreach { line =>
      val parsedLine = parseLine(line)
      val pos        = move(parsedLine)
      if (blacks.contains(pos)) blacks.remove(pos) else blacks.add(pos)
    }

    (1 to 100).foreach { turn =>
      val allPos = for {
        x <- -(turn + 30) to (turn + 30)
        y <- -(turn + 30) to (turn + 30)
        z <- -(turn + 30) to (turn + 30)
        if (x + y + z) == 0
      } yield (x, y, z)

      val newBlack = mutable.Set[(Int, Int, Int)]()

      allPos.foreach { p =>
        val count = neighbors(p).count(blacks)
        if (blacks.contains(p)) {
          // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
          if (count == 0 || count > 2) {
            // flipped to white
          } else {
            newBlack.add(p)
          }
        } else {
          // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
          if (count == 2)
            newBlack.add(p)
        }

      }
      blacks = newBlack
    }
    blacks.size
  }

  def neighbors(p: (Int, Int, Int)): Seq[(Int, Int, Int)] = {
    val vectors = Seq(
      (-1, 1, 0),
      (1, -1, 0),
      (0, -1, 1),
      (-1, 0, 1),
      (1, 0, -1),
      (0, 1, -1)
    )
    vectors.map(v => (v._1 + p._1, v._2 + p._2, v._3 + p._3))
  }
}
