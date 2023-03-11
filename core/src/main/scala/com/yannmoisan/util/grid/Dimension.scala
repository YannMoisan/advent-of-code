package com.yannmoisan.util.grid

import scala.collection.mutable

sealed abstract case class Dimension(width: Int, height: Int) {
  // for perf reason (cpu cache), order matters here
  val allPos = for {
    y <- 0 until height
    x <- 0 until width
  } yield Pos(x, y)(this)

  private val neighborsCache: Array[Array[Pos]] = {
    val cache = Array.ofDim[Array[Pos]](width * height)
    allPos.foreach { pos =>
      cache(pos.index) = Seq(
        Pos(pos.x + 1, pos.y)(this),
        Pos(pos.x - 1, pos.y)(this),
        Pos(pos.x, pos.y + 1)(this),
        Pos(pos.x, pos.y - 1)(this)
      ).filter(p => p.x >= 0 && p.x < width && p.y >= 0 && p.y < height).toArray

    }
    cache
  }

  private val neighbors8Cache: Array[Array[Pos]] = {
    val cache = Array.ofDim[Array[Pos]](width * height)
    allPos.foreach { pos =>
      cache(pos.index) = Seq(
        Pos(pos.x + 1, pos.y)(this),
        Pos(pos.x - 1, pos.y)(this),
        Pos(pos.x, pos.y + 1)(this),
        Pos(pos.x, pos.y - 1)(this),
        Pos(pos.x + 1, pos.y + 1)(this),
        Pos(pos.x + 1, pos.y - 1)(this),
        Pos(pos.x - 1, pos.y + 1)(this),
        Pos(pos.x - 1, pos.y - 1)(this)
      ).filter(p => p.x >= 0 && p.x < width && p.y >= 0 && p.y < height).toArray

    }
    cache
  }

  def neighbors(p: Pos): Array[Pos] = neighborsCache(p.index)

  def neighbors8(p: Pos): Array[Pos] = neighbors8Cache(p.index)

  // The grid is torus-shaped, meaning exiting the grid from one side will wrap you round to the other side.
  private val moveCache = {
    val arr = Array.ofDim[Pos](4 * width * height)
    for {
      dir <- Seq(Direction.Up, Direction.Down, Direction.Left, Direction.Right)
      x <- 0 until width
      y <- 0 until height
    } {
      arr(dir.value + 4 * (x + width * y)) = {
        dir match {
          case Direction.Up   => Pos(x, if (y == 0) height - 1 else y - 1)(this)
          case Direction.Down => Pos(x, (y + 1) % height)(this)
          case Direction.Left => Pos(if (x == 0) width - 1 else x - 1, y)(this)
          case Direction.Right => Pos((x + 1) % width, y)(this)
        }
      }
    }
    arr
  }

  def move(pos: Pos, dir: Direction): Pos = {
    moveCache(dir.value + 4 * (pos.x + width * pos.y))
  }

  def move(code: Int): Pos = {
    moveCache(code)
  }

  def move(p: Pos, dir: (Int, Int)): Option[Pos] = {
    val newp = Pos(p.x + dir._1, p.y + dir._2)(Dimension(width, height))
    if (newp.x >= 0 && newp.y >= 0 && newp.x < width && newp.y < height) Some(newp) else None
  }

}

// Use only in a single-threaded context
object Dimension {
  private val cache = mutable.Map[(Int, Int), Dimension]()

  def apply(width: Int, height: Int): Dimension = {
    cache.getOrElseUpdate((width, height), new Dimension(width, height) {})
  }
}
