import scala.reflect.ClassTag

class Grid[T: ClassTag](g: Array[Array[T]]) {
  def apply(p: (Int, Int)): T = g(p._1)(p._2)

  def update(p: (Int, Int), x: T): Unit = g(p._1)(p._2) = x

  def indices: Seq[(Int, Int)] =
    for {
      r <- 0 until g.length
      c <- 0 until g.head.length
    } yield (r, c)

  def row(r: Int): Array[T] = g(r)

  def col(c: Int): Array[T] = Array.tabulate(g.head.length)(i => g(i)(c))

  def all: Iterator[T] = g.iterator.flatMap(_.iterator)

}
