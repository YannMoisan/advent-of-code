import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

import scala.collection.mutable

object Day17 extends SinglePuzzle[Int, Long] {
  private val MaxHeight = 40000

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

  case class Key(rockMod: Int, directionMod: Int)
  case class Value(firstEmptyLine: Int, rockId: Int)
  val cache = mutable.Map[Key, Value]()

  override def part1(input: String): Int = {
    val grid = Grid1D.fill(7, MaxHeight)('.')

    val directions = input

    /* A new rock begins falling:
    Jet of gas pushes rock left:
    Rock falls 1 unit:
    +-------+*/

    // is falling possible ?
    Iterator
      .iterate((MaxHeight - 1, 0, 0))(t => fall(t._3, grid, t._1, directions, t._2)).drop(2022).next()

    MaxHeight - (0 to grid.dim.height - 1).reverse
      .find(y => (0 until 7).forall(x => grid(Pos(x, y)) == '.')).get - 1
  }

  // cycle detected key=Key(0,9976), old=Value(37290,1715), new=Value(24190,10265), #rocks=-8550, #height=13100
  // scala> 1000000000000L % 8550
  // val res0: Long = 2800
  //
  // scala> 1000000000000L / 8550
  // val res1: Long = 116959064

  override def part2(input: String): Long = {
    val grid = Grid1D.fill(7, MaxHeight)('.')

    val directions = input

    /* A new rock begins falling:
    Jet of gas pushes rock left:
    Rock falls 1 unit:
    +-------+*/

    // is falling possible ?
    Iterator
      .iterate((MaxHeight - 1, 0, 0))(t => fall(t._3, grid, t._1, directions, t._2)).drop(2800).next()

    ((1000000000000L / 8550) * 13100) + MaxHeight - (0 to grid.dim.height - 1).reverse
      .find(y => (0 until 7).forall(x => grid(Pos(x, y)) == '.')).get - 1
  }

  private def fall(
      rockId: Int,
      grid: Grid[Char],
      firstEmptyLine: Int,
      directions: String,
      dirIndex: Int
  ): (Int, Int, Int) = {
    // Each rock appears so that its left edge is two units away from the left wall
    // and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
    var current = Pos(2, firstEmptyLine - 3)

    val k = Key(rockId % 5, dirIndex % directions.length)
    val v = Value(firstEmptyLine, rockId)
    if (cache.contains(k)) {
      println(
        s"cycle detected key=$k, old=${cache(k)}, new=$v, #rocks=${cache(k).rockId - v.rockId}, #height=${cache(k).firstEmptyLine - firstEmptyLine}"
      )
    } else cache.put(k, v)

    val rock = pieces(rockId % 5)

    var continue  = true
    var dirIndex0 = dirIndex
    while (continue) {
      val dir = directions(dirIndex0 % directions.size)
      dirIndex0 += 1
      val xOffset = if (dir == '>') +1 else -1
      if (canMove(rock, grid, Pos(current.x + xOffset, current.y))) {
        current = Pos(current.x + xOffset, current.y)
      }
      if (canMove(rock, grid, Pos(current.x, current.y + 1))) {
        current = Pos(current.x, current.y + 1)
      } else continue = false
    }

    rest(rock, grid, current)
    (math.min(firstEmptyLine, current.y - rock.length), dirIndex0, rockId + 1)
  }

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
