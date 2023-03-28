package com.yannmoisan.util.grid

import scala.reflect.ClassTag

// Design choices
// Position must be cached.
// The cache must be shared between grid instances.
// tradeoff. Speed over type safety. Array[Int] is faster than Array[Pos]

class Grid2D[A: ClassTag](val grid: Array[Array[A]]) extends Grid[A] {
  override val dim: Dimension = Dimension(grid.head.length, grid.length)

  override def apply(index: Int): A = {
    val pos = dim.positions(index)
    grid(pos.y)(pos.x)
  }

  override def update(p: Int, ch: A): Unit = {
    val pos = dim.positions(p)
    grid(pos.y)(pos.x) = ch
  }

  def count(f: A => Boolean): Int = grid.map(_.count(f)).sum

  def copy(): Grid2D[A] =
    new Grid2D(grid.map(_.clone()))

  def debug(): Unit =
    grid.foreach(line => Console.err.println(line.mkString))

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Grid2D[_] =>
        dim.indices.forall(p => apply(p) == other(p))
      case _ => false
    }
}

object Grid2D {
  def fill[A: ClassTag](width: Int, height: Int)(elem: => A): Grid2D[A] =
    new Grid2D(Array.fill(height)(Array.fill(width)(elem)))
}
