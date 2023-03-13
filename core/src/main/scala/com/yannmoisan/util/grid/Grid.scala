package com.yannmoisan.util.grid

trait Grid[A] {
  def dim: Dimension

  def apply(p: Pos): A

  def update(p: Pos, ch: A): Unit

  def find(ch: A): Option[Pos] = {
    dim.allPos.find(apply(_) == ch)
  }

  def findAll(ch: A): Seq[Pos] = {
    dim.allPos.toIndexedSeq.filter(apply(_) == ch)
  }

  def count(f: A => Boolean): Int

}
