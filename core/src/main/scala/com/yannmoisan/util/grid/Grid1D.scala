package com.yannmoisan.util.grid

import scala.reflect.ClassTag

class Grid1D[@specialized(Int, Char, Boolean) A](
    private val grid: Array[A],
    width: Int,
    height: Int
) extends Grid[A] {
  override val dim: Dimension = Dimension(width, height)

  def copy(): Grid1D[A] = {
    new Grid1D(grid.clone(), width, height)
  }

  override def apply(index: Int): A = grid(index)

  override def update(index: Int, ch: A): Unit = grid(index) = ch

  override def count(f: A => Boolean): Int = grid.count(f)

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Grid1D[_] =>
        var i = 0
        var ret = true
        while (ret && i < dim.allPos.length) {
          val p = dim.allPos(i)
          if (this.apply(p.index) != other(p.index)) ret = false
          i += 1
        }
        ret

      case _ => false
    }
}

object Grid1D {
  def fill[@specialized(Int, Char, Boolean) A: ClassTag](
      width: Int,
      height: Int
  )(elem: => A): Grid1D[A] =
    new Grid1D(Array.fill(width * height)(elem), width, height)

  def apply[@specialized(Int, Char, Boolean) A: ClassTag](
      grid: Array[Array[A]]
  ): Grid1D[A] = {
    new Grid1D(grid.flatten, grid.head.length, grid.length)
  }

  def apply(grid: Array[String]): Grid1D[Char] = {
    new Grid1D(grid.flatten, grid.head.length, grid.length)
  }
}
