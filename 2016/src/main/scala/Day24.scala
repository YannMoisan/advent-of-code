import com.yannmoisan.util.grid.{Grid, Grid1D}

object Day24 extends MultiPuzzle[Int, Int] {

  def moves: State => Seq[State] =
    s => s.grid.dim.neighbors4(s.p).filter(s.grid(_) != '#').map(p => State(s.grid, p))

  case class State(grid: Grid[Char], p: Int)

  // no need to convert char to int
  def findNumbers(grid: Grid[Char]): Seq[(Int, Int)] =
    grid.dim.indices.collect {
      case i if (grid(i) != '.' && grid(i) != '#') => (grid(i).asDigit, i)
    }

  def dist(grid: Grid[Char], p1: Int, p2: Int): Int = {
    val init  = State(grid, p1)
    val nodes = BFS.breadth_first_traverse(init, moves)
    nodes.find(_._1.p == p2).get._2.size - 1
  }

  type Path = Seq[(Int, Int)]

  def pathDist(path: Path, dists: Dists): Int = path.map(dists).sum

  def paths(mapping: Map[Int, Int], shouldReturn: Boolean): Seq[Path] = {
    val startBy0 = (0 to 7)
      .map(mapping)
      .permutations
      .filter(_(0) == mapping(0))

    val withReturn = if (shouldReturn) startBy0.map(l => l :+ mapping(0)) else startBy0

    withReturn
      .map(_.sliding(2))
      .map(_.map(l => (l(0), l(1))))
      .map(_.toList).toList
  }

  type Dists = Map[(Int, Int), Int]

  def computeDists(grid: Grid[Char], mapping: Map[Int, Int]): Dists = {
    val combinations: Iterator[IndexedSeq[Int]] = (0 to 7).map(mapping).combinations(2)
    val paired: Iterator[(Int, Int)]            = combinations.map(l => (l(0), l(1)))
    val ds: Map[(Int, Int), Int]                = paired.map(t => (t, dist(grid, t._1, t._2))).toMap
    ds ++ ds.toList.map { case (t, d) => ((t._2, t._1), d) }.toMap
  }

  def part(shouldReturn: Boolean) = { lines: Array[String] =>
    val grid                   = Grid1D(lines)
    val numbers: Map[Int, Int] = findNumbers(grid).toMap
    val dists                  = computeDists(grid, numbers)
    paths(numbers, shouldReturn).map(p => pathDist(p, dists)).min
  }

  override def part1(lines: Iterator[String]) = part(false)(lines.toArray)

  override def part2(lines: Iterator[String]) = part(true)(lines.toArray)
}
