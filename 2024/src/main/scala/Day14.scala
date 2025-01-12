import com.yannmoisan.util.grid.{Dimension, DirectionWithIndex, Grid1D, Pos, TorusShapedMove}

object Day14 extends MultiPuzzle[Int, Int] {
  case class Robot(x: Int, y: Int, vx: Int, vy: Int)

  override def part1(input: Iterator[String]): Int = {
    val robots: Seq[(Int, Int, Int, Int)] = input.map { s =>
      val s"p=$x,$y v=$vx,$vy" = s
      (x.toInt, y.toInt, vx.toInt, vy.toInt)
    }.toList

    val dim = Dimension(101, 103)
    val end = Iterator.iterate(robots)(next(dim)).drop(100).next()
    end
      .collect { case (x, y, _, _) => quadrant(x, y, dim) }
      .filter(_.isDefined)
      .groupMapReduce(identity)(_ => 1)(_ + _).values
      .product

  }

  def print(dim: Dimension)(robots: Seq[(Int, Int, Int, Int)]): Unit = {
    val g = Grid1D.tabulate(dim)(_ => ' ')
    robots.foreach(r => g(g.dim.index(Pos(r._1, r._2))) = '*')
    g.debug()
  }

  def next(dim: Dimension)(robots: Seq[(Int, Int, Int, Int)]): Seq[(Int, Int, Int, Int)] = {
    val move = new TorusShapedMove(dim) {}
    robots.map { robot =>
      val optPos = move
        .move(
          dim.index(Pos(robot._1, robot._2)),
          new DirectionWithIndex(robot._3, robot._4, -1) {}
        ).get
      (dim.pos(optPos).x, dim.pos(optPos).y, robot._3, robot._4)
    }
  }

  def quadrant(x: Int, y: Int, dim: Dimension): Option[(Boolean, Boolean)] = {
    val pivotX = dim.width / 2
    val pivotY = dim.height / 2
    if (x == pivotX || y == pivotY) None
    else Some((x < pivotX, y < pivotY))
  }

  // p=67,43 v=80,86

  override def part2(input: Iterator[String]): Int = {
    val robots: Seq[(Int, Int, Int, Int)] = input.map { s =>
      val s"p=$x,$y v=$vx,$vy" = s
      (x.toInt, y.toInt, vx.toInt, vy.toInt)
    }.toList

    val dim = Dimension(101, 103)
    var i   = 0
    var cur = robots
    while (i < 10000) {
      i += 1

      cur = next(dim)(cur)
      if ((i - 13) % 101 == 0) {
        println(s"---$i---")
        print(dim)(cur)
      }
    }
    7083
    //Iterator.iterate(robots)(next(dim)).drop(100).next()
  }

  // 7083
}
