package com.yannmoisan.util.grid

import scala.reflect.ClassTag

class Grid1D[A: ClassTag](grid: Array[A], width: Int, height: Int)
    extends Grid[A] {
  def this(grid: Array[Array[A]]) = {
    this(grid.flatten, grid.head.length, grid.length)
  }

  override def apply(p: Pos): A = grid(p.index)

  override def update(p: Pos, ch: A): Unit = grid(p.index) = ch

  override val dim: Dimension = Dimension(width, height)
}
