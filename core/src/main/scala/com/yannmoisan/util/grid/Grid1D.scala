package com.yannmoisan.util.grid

//import scala.reflect.ClassTag

class Grid1D[@specialized(Int, Char, Boolean) A](grid: Array[A], width: Int, height: Int)
    extends Grid[A] {
//  def this(grid: Array[Array[A]]) = {
//    this(grid.flatten, grid.head.length, grid.length)
//  }

  override def apply(p: Pos): A = grid(p.index)

  def apply(index: Int): A = grid(index)

  def count(f: A => Boolean): Int = grid.count(f)

  override def update(p: Pos, ch: A): Unit = grid(p.index) = ch

   def update(index: Int, ch: A): Unit = grid(index) = ch

  override val dim: Dimension = Dimension(width, height)
}
