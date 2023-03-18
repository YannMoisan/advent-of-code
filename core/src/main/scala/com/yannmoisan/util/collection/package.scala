package com.yannmoisan.util

package object collection {
  def next[A](dir: A, dirs: Seq[A]): A = {
    val i = dirs.indexOf(dir) + 1
    dirs(i % dirs.size)
  }

  def prev[A](dir: A, dirs: Seq[A]): A = {
    val i = dirs.indexOf(dir) - 1
    dirs((i + dirs.size) % dirs.size)
  }
}
