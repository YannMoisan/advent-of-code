package com.yannmoisan.util.grid

trait Grid[@specialized(Int, Char, Boolean) A] {
  def dim: Dimension

  def apply(index: Int): A
  def apply(pos: Pos): A = apply(dim.index(pos))

  def update(p: Int, ch: A): Unit
  def update(pos: Pos, ch: A): Unit = update(dim.index(pos), ch)

  def find(ch: A): Option[Int] = {
    var i   = 0
    val len = dim.width * dim.height
    while (i < len) {
      if (apply(i) == ch) return Some(i)
      i += 1
    }
    None
  }

  def findAll(ch: A): Seq[Int] =
    dim.indices.filter(p => apply(p) == ch)

  def count(f: A => Boolean): Int

  def copy(): Grid[A]
}
