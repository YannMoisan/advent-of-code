import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day17 extends SinglePuzzle[Int, Int] {
  val pieces: Seq[Array[String]] = Seq(
    """####""",
    """.#.
      |###
      |.#.""".stripMargin,
    """..#
      |..#
      |###""".stripMargin,
    """#
      |#
      |#
      |#""".stripMargin,
    """##
      |##""".stripMargin
  ).map(_.split("\n"))

  override def part1(input: String): Int = {
    val grid = Grid1D.fill(7, 4000)('.')

    //val strDirections              = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    val directions: Iterator[Char] = Iterator.continually(input.toCharArray).flatten

    val rocks = Iterator.continually(pieces).flatten
    /* A new rock begins falling:
    Jet of gas pushes rock left:
    Rock falls 1 unit:
    +-------+*/

    // is falling possible ?
    (0 until 2022).foreach(_ => fall(rocks.next(), grid, directions))
//    grid.debug()

    4000 - (0 to grid.dim.height - 1).reverse
      .find(y => (0 until 7).forall(x => grid(Pos(x, y)) == '.')).get - 1
  }

  override def part2(input: String): Int = 42

  private def fall(rock: Array[String], grid: Grid[Char], directions: Iterator[Char]): Unit = {
    val firstEmptyLine = (0 to grid.dim.height - 1).reverse
      .find(y => (0 until 7).forall(x => grid(Pos(x, y)) == '.')).get

    // Each rock appears so that its left edge is two units away from the left wall
    // and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
    var current = Pos(2, firstEmptyLine - 3)

    var continue = true

    while (continue) {
      val dir     = directions.next()
      val xOffset = if (dir == '>') +1 else -1
      if (canMove(rock, grid, Pos(current.x + xOffset, current.y))) {
        current = Pos(current.x + xOffset, current.y)
      }
      if (canMove(rock, grid, Pos(current.x, current.y + 1))) {
        current = Pos(current.x, current.y + 1)
      } else continue = false
    }
//    val xOffset = if (directions.next() == '>') +1 else -1
//    if (canMove(rock, grid, Pos(current.x + xOffset, current.y))) {
//      current = Pos(current.x + xOffset, current.y)
//    }
    rest(rock, grid, current)
  }

  // 0123456
  private def canMove(rock: Array[String], grid: Grid[Char], target: Pos): Boolean =
    if (target.y == grid.dim.height || target.x < 0 || target.x + rock.head.length > 7)
      false
    else {
      val height = rock.length
      val width  = rock.head.length
      val rockPositions = (for {
        x <- 0 until width
        y <- 0 until height
        if rock(y)(x) == '#'
      } yield Pos(x, y - (height - 1)))
      rockPositions.forall(pos => grid(Pos(target.x + pos.x, target.y + pos.y)) == '.')
    }

  private def rest(rock: Array[String], grid: Grid[Char], target: Pos): Unit = {
    val height = rock.length
    val width  = rock.head.length
    val rockPositions = (for {
      x <- 0 until width
      y <- 0 until height
      if rock(y)(x) == '#'
    } yield Pos(x, y - (height - 1)))
    rockPositions.foreach(pos => grid(Pos(target.x + pos.x, target.y + pos.y)) = '#')
  }

}
