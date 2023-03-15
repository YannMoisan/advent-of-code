import com.yannmoisan.util.grid.{Direction, Grid1D}
import fp.Functional

// Double buffering
// reuse array
@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day25 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val cur = Grid1D(input.toArray.map(_.toCharArray))
    new Functional[Grid1D[Char]]().convergesIn(next, cur, _.equals(_)) + 1
  }

  override def part2(input: Iterator[String]): Int = 42

  def `next>`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.indices.foreach { p =>
      val pn = g.dim.move(p, Direction.Right).get
      if (g(p) == '>' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = '>'
      }
    }
    n
  }

  def `nextv`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.indices.foreach { p =>
      val pn = g.dim.move(p, Direction.Down).get
      if (g(p) == 'v' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = 'v'
      }
    }
    n
  }

  def next(g: Grid1D[Char]): Grid1D[Char] = nextv(`next>`(g))
}
