import com.yannmoisan.util.grid.{Grid1D, Pos}

// Double buffering
// reuse array
object Day25 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val cur = Grid1D(input.toArray.map(_.toCharArray))
    new Functional[Grid1D[Char]]().convergesIn(next, cur, _.equals(_)) + 1
  }

  override def part2(input: Iterator[String]): Int = 42

  def `next>`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.allPos.foreach { p =>
      val pn = Pos((p.x + 1) % g.dim.width,p.y)(g.dim)
      if (g(p) == '>' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = '>'
      }
    }
    n
  }

  def `nextv`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.allPos.foreach { p =>
      val pn = Pos(p.x,(p.y + 1) % g.dim.height)(g.dim)
      if (g(p) == 'v' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = 'v'
      }
    }
    n
  }

  def next(g: Grid1D[Char]): Grid1D[Char] = nextv(`next>`(g))
}
