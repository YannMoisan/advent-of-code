package com.yannmoisan.util.grid

import scala.reflect.ClassTag

class Grid1D[@specialized(Int, Char, Boolean) A](
    private val grid: Array[A],
    width: Int,
    height: Int
) extends Grid[A] {
  override val dim: Dimension = Dimension(width, height)

  override def apply(index: Int): A = grid(index)

  override def update(index: Int, ch: A): Unit = grid(index) = ch

  override def count(f: A => Boolean): Int = grid.count(f)

  def debug(): Unit =
    grid.grouped(dim.width).foreach(line => Console.err.println(line.mkString))

  def copy(): Grid[A] =
    new Grid1D(grid.clone(), width, height)

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Grid1D[_] =>
        var i   = 0
        var ret = true
        while (ret && i < dim.positions.length) {
          if (this.apply(i) != other(i)) ret = false
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
  ): Grid1D[A] =
    new Grid1D(grid.flatten, grid.head.length, grid.length)

  def apply(grid: Iterator[String]): Grid1D[Char] = {
    val arr = grid.toArray
    new Grid1D(arr.flatten, arr.head.length, arr.length)
  }

  def tabulate[@specialized(Int, Char, Boolean) A: ClassTag](
      dim: Dimension
  )(f: Int => A): Grid1D[A] = {
    val arr = Array.tabulate(dim.width * dim.height)(f)
    new Grid1D(arr, dim.width, dim.height)
  }
}
