package com.yannmoisan.util.grid

import scala.reflect.ClassTag

class Grid1D[@specialized(Int, Char, Boolean) A](grid: Array[A], width: Int, height: Int)
    extends Grid[A] {

  def copy(): Grid1D[A] = {
    new Grid1D(grid.clone(), width, height)
  }

  override def apply(p: Pos): A = grid(p.index)

  def apply(index: Int): A = grid(index)

  def count(f: A => Boolean): Int = grid.count(f)

  override def update(p: Pos, ch: A): Unit = grid(p.index) = ch

   def update(index: Int, ch: A): Unit = grid(index) = ch

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Grid1D[_] => dim.allPos.forall(p => apply(p) == other(p))
      case _ => false
    }


  override val dim: Dimension = Dimension(width, height)
}

object Grid1D {
    def apply[A:ClassTag](grid: Array[Array[A]]): Grid1D[A] = {
      new Grid1D(grid.flatten, grid.head.length, grid.length)
    }
}
