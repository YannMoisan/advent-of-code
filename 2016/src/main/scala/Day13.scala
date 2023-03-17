object Day13 extends SinglePuzzle[Int, Int] {

  type Grid = Seq[String]

  case class Pos(x: Int, y: Int)

  def possibleMoves(p: Pos): Seq[Pos] = Seq(
    Pos(p.x - 1, p.y),
    Pos(p.x + 1, p.y),
    Pos(p.x, p.y - 1),
    Pos(p.x, p.y + 1)
  )

  def validPos(g: Grid)(p: Pos) =
    p.x >= 0 &&
      p.x < g(0).length &&
      p.y >= 0 &&
      p.y < g.size &&
      g(p.y)(p.x) == '.'

  val grid = generateMaze(100, 100, 1350)

  def moves: Pos => Seq[Pos] = p => possibleMoves(p).filter(validPos(grid))

  def generateMaze(w: Int, h: Int, fav: Int): Grid =
    (0 until h).map(y => (0 until w).map(x => isNumber(x, y, fav)).mkString(""))

  def print(g: Grid) = g.foreach(println)

  def isNumber(x: Long, y: Long, fav: Long): Char =
    if ((x * x + 3 * x + 2 * x * y + y + y * y + fav).toBinaryString.count(_ == '1') % 2 == 0) '.'
    else '#'

  override def part1(s: String): Int = {
    val stream = BFS.breadth_first_traverse(Pos(1, 1), moves)
    stream.find(_._1 == Pos(31, 39)).get._2.length - 1
  }

  override def part2(s: String): Int = {
    val stream            = BFS.breadth_first_traverse(Pos(1, 1), moves)
    val limitedStream     = stream.takeWhile(_._2.length <= 51)
    val visited: Set[Pos] = limitedStream.map(_._1).toSet
    visited.size
  }

}
