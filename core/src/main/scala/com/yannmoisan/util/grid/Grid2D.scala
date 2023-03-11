package com.yannmoisan.util.grid

import scala.reflect.ClassTag

// Design choices
// Position must be cached.
// The cache must be shared between grid instances.

class Grid2D[A: ClassTag](val grid: Array[Array[A]]) extends Grid[A] {

  def copy(): Grid2D[A] = {
    new Grid2D(grid.map(_.clone()))
  }

  def apply(p: Pos): A = grid(p.y)(p.x)

  def update(p: Pos, ch: A): Unit = grid(p.y)(p.x) = ch

  def debug(): Unit =
    grid.foreach(line => Console.err.println(line.mkString))

  override val dim: Dimension = Dimension(grid.head.length, grid.length)

    override def equals(obj: Any): Boolean =
      obj match {
        case other: Grid2D[_] => dim.allPos.forall(p => apply(p) == other(p))
        case _           => false
      }

}

object Grid2D {
  def fill[A: ClassTag](width: Int, height: Int)(elem: => A) : Grid2D[A] = new Grid2D(Array.fill(height)(Array.fill(width)(elem)))
}
