import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.Direction4

object Day3 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = positions(input).size

  override def part2(input: String): Int =
    (positions(input.sliding(1, 2).mkString) ++
      positions(input.tail.sliding(1, 2).mkString)).size

  private def positions(input: String): Set[Position] =
    input
      .scanLeft(Position(0, 0)) { (pos, ch) =>
        val dir = ch match {
          case '^' => Direction4.Up
          case 'v' => Direction4.Down
          case '<' => Direction4.Left
          case '>' => Direction4.Right
        }
        Position.move(pos, dir)
      }.toSet
}

//scala> "ABCDEFGHI".sliding(1,2).mkString
//val res1: String = ACEGI
//
//scala> "ABCDEFGHI".tail.sliding(1,2).mkString
//val res2: String = BDFH

//>>> "ABCDEFGHI"[::2]
//'ACEGI'
//>>> "ABCDEFGHI"[1::2]
//'BDFH'
