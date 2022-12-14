case class Pos(x: Int, y: Int)

class Grid(private val grid: Array[Array[Char]]) {
  def width = grid.head.length
  def height = grid.length
  def xindices: Seq[Int] = grid.head.indices
  def yindices: Seq[Int] = grid.indices
  def apply(p: Pos) : Char = grid(p.y)(p.x)
  def isInRange(p: Pos) : Boolean = p.x >= 0 && p.x < width && p.y >= 0 && p.y < height
  def posOf(c: Char) : Option[Pos] = allPos.find(p => apply(p) == c)
  def update(p: Pos, c: Char): Unit = grid(p.y)(p.x) = c
  def count(p: Char => Boolean) : Int = allPos.count(pos => p(apply(pos)))
  private def allPos: Seq[Pos] = for {
    x <- xindices
    y <- yindices
  } yield Pos(x, y)
}
