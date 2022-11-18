
object Day8 extends MultiPuzzle[Int, String] {
  type Screen = Seq[Seq[Char]]
  type Transfo = Screen => Screen

  val (x, y) = (50, 6)
  val regexRotateRow = """rotate row y=(\d+) by (\d+)""".r
  val regexRotateCol = """rotate column x=(\d+) by (\d+)""".r
  val regexRect = """rect (\d+)x(\d+)""".r

  def part = { lines: Seq[String] =>
    val transfos: Seq[Transfo] = lines.collect {
      case regexRotateRow(y, by) => rotateRow(y.toInt, by.toInt) _
      case regexRotateCol(x, by) => rotateCol(x.toInt, by.toInt) _
      case regexRect(x, y) => rect(x.toInt, y.toInt) _
    }

    val screen = init(x, y)
    transfos.foldLeft(screen) { case (acc, t) => t.apply(acc) }
  }

  // TODO x => f(g(x)) point-free
  override def part1(lines: Iterator[String]): Int = count(part(lines.toList))

  override def part2(lines: Iterator[String]) : String = {
    print(part(lines.toList))
    "EOARGPHYAO"
  }

  def init(x: Int, y: Int): Screen = List.fill(y)(List.fill(x)('.'))

  def updated(screen: Screen, x: Int, y: Int) = screen.updated(y, screen(y).updated(x, 'X'))

  def rect(w: Int, h: Int)(screen: Screen) = {
    val coords = for {
      x <- 0 until w
      y <- 0 until h
    } yield (x, y)
    coords.foldLeft(screen) { case (acc, e) => updated(acc, e._1, e._2) }
  }

  def rotateRow(y: Int, by: Int)(screen: Screen) =
    screen.updated(y, rotate(screen(y), by))

  def rotateCol(x: Int, by: Int)(screen: Screen) =
    rotateRow(x, by)(screen.transpose).transpose

  def print(screen: Screen) = screen.foreach { l => println(l.mkString("")) }

  def rotate[A](as: Seq[A], by: Int) = as.takeRight(by) ++ as.take(as.length - by)

  def count(screen: Screen) = screen.flatten.count(_ == 'X')
}
