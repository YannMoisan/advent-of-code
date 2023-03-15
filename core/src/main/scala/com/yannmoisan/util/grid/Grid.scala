package com.yannmoisan.util.grid

trait Grid[@specialized(Int, Char, Boolean) A] {
  def dim: Dimension

  def apply(index: Int): A

  def update(p: Int, ch: A): Unit

  def find(ch: A): Option[Int] = {
    var i = 0
    val len = dim.width * dim.height
    while (i < len) {
      if (apply(i) == ch) return Some(i)
      i += 1
    }
    None
  }

  def findAll(ch: A): Seq[Int] = {
    dim.positions.toIndexedSeq.filter(p => apply(p.index) == ch).map(_.index)
  }

  def count(f: A => Boolean): Int
}
