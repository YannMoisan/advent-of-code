package com.yannmoisan.util

import scala.collection.mutable

package object collection {
  def next[A](dir: A, dirs: Seq[A]): A = {
    val i = dirs.indexOf(dir) + 1
    dirs(i % dirs.size)
  }

  def prev[A](dir: A, dirs: Seq[A]): A = {
    val i = dirs.indexOf(dir) - 1
    dirs((i + dirs.size) % dirs.size)
  }

  def firstDuplicate[T](it: Iterator[T]): Option[T] = {
    val visited = mutable.Set[T]()
    while (it.hasNext) {
      val cur = it.next()
      if (visited.contains(cur)) return Some(cur)
      visited.add(cur)
    }
    None
  }

  def firstDuplicateIndex[T](it: Iterator[T]): Option[Int] = {
    val it2     = it.zipWithIndex
    val visited = mutable.Set[T]()
    while (it2.hasNext) {
      val (cur, idx) = it2.next()
      if (visited.contains(cur)) return Some(idx)
      visited.add(cur)
    }
    None
  }

  def firstConsecutiveDuplicate[T](it: Iterator[T]): Option[T] =
    it.sliding(2).find(seq => seq(0) == seq(1)).map(_.apply(0))

}
