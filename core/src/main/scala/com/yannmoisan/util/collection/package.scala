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
}
