// Double buffering
// reuse array
object Day25 extends MultiPuzzle[Int, Int] {
  type Grid = Array[Array[Char]]

  override def part1(input: Iterator[String]): Int = {
    val cur: Grid = input.toArray.map(_.toCharArray)
    new Functional[Grid]().convergesIn(next, cur, eql) + 1
  }

  override def part2(input: Iterator[String]): Int = 42

  def clone(g: Grid): Grid =
    Array.tabulate(g.length, g.head.length) { case (r, c) => g(r)(c) }

  def print(g: Grid): Unit =
    g.foreach(row => println(String.valueOf(row)))

  def `next>`(g: Grid): Grid = {
    val n = clone(g)
    for {
      r <- 0 until g.length
      c <- 0 until g.head.length
    } {
      val cn = (c + 1) % g.head.length
      if (g(r)(c) == '>' && g(r)(cn) == '.') {
        n(r)(c) = '.'
        n(r)(cn) = '>'
      }
    }
    n
  }

  def `nextv`(g: Grid): Grid = {
    val n = clone(g)
    for {
      r <- 0 until g.length
      c <- 0 until g.head.length
    } {
      val rn = (r + 1) % g.length
      if (g(r)(c) == 'v' && g(rn)(c) == '.') {
        n(r)(c) = '.'
        n(rn)(c) = 'v'
      }
    }
    n
  }

  def next(g: Grid): Grid = nextv(`next>`(g))

  def eql(a: Grid, b: Grid): Boolean = {
    val a1 = a.toList.map(String.valueOf)
    val b1 = b.toList.map(String.valueOf)
    a1 == b1
  }

}
