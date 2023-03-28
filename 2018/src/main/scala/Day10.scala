import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.{Direction, Grid1D, Pos}

object Day10 extends MultiPuzzle[Int, Int] {
  case class Point(p: Position, v: Direction)
  override def part1(input: Iterator[String]): Int = {
    val pointsF = iterator(input)
      .sliding(2)
      .find(seq => dim(corners(seq(0)))._1 < dim(corners(seq(1)))._1).get.apply(0)

    val cornerz = corners(pointsF)

    val grid = Grid1D.fill(dim(cornerz)._1, dim(cornerz)._2)(' ')
    pointsF.foreach(p =>
      grid(Pos(p.p.x - cornerz._1.x, p.p.y - cornerz._1.y)(grid.dim).index) = 'X'
    )
    grid.debug()
    // AJZNXHKE
    42
  }

  private def corners(points: Array[Point]): (Position, Position) = {
    val minX = points.minBy(_.p.x).p.x
    val maxX = points.maxBy(_.p.x).p.x
    val minY = points.minBy(_.p.y).p.y
    val maxY = points.maxBy(_.p.y).p.y
    (Position(minX, minY), Position(maxX, maxY))
  }

  private def dim(corners: (Position, Position)): (Int, Int) =
    (corners._2.x - corners._1.x + 1, corners._2.y - corners._1.y + 1)

  override def part2(input: Iterator[String]): Int =
    iterator(input).zipWithIndex
      .sliding(2)
      .find(seq => dim(corners(seq(0)._1))._1 < dim(corners(seq(1)._1))._1)
      .get.apply(0)._2

  private def iterator(input: Iterator[String]) = {
    val points = input.map {
      case s"position=<$px,$py> velocity=<$vx,$vy>" =>
        Point(
          Position(px.trim.toInt, py.trim.toInt),
          new Direction(vx.trim.toInt, vy.trim.toInt)
        )
    }.toArray
    Iterator
      .iterate(points)(_.map(p => Point(Position.move(p.p, p.v), p.v)))
  }
}
