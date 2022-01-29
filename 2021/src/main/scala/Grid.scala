import scala.reflect.ClassTag

class Grid[T: ClassTag](g: Array[Array[T]]) {
  def rowCount = g.length
  def colCount = g.head.length

  def apply(p: (Int, Int)): T = g(p._1)(p._2)

  def update(p: (Int, Int), x: T): Unit = g(p._1)(p._2) = x

  def indices: Seq[(Int, Int)] =
    for {
      r <- 0 until rowCount
      c <- 0 until colCount
    } yield (r, c)

  def row(r: Int): Array[T] = g(r)

  def col(c: Int): Array[T] = Array.tabulate(g.head.length)(i => g(i)(c))

  def all: Iterator[T] = g.iterator.flatMap(_.iterator)

  def neighbors(p: (Int, Int)): Seq[(Int, Int)] =
    for {
      dr <- -1 to 1
      dc <- -1 to 1
      r = p._1 + dr
      c = p._2 + dc
      if dr != 0 || dc != 0
      if r >= 0 && c >= 0 && r < rowCount && c < rowCount
    } yield (r, c)
}
