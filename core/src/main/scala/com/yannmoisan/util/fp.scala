package com.yannmoisan.util

object fp {

  import scala.annotation.tailrec

  @tailrec
  def loop[A] (s: A) (f: A => A, stop: A => Boolean): A = {
  val newS = f(s)
  if (stop (newS) ) newS else loop (newS) (f, stop)
}

  // alternative impl with Iterator
  def loop2[A](s: A)(f: A => A, stop: A => Boolean): A = {
    Iterator.iterate(s)(f).find(stop).get
  }
}

// Showcase some usages of Iterator
// index are 0-based, so the 4th element is at index 3
object IteratorApp extends App {
  def powersOf2 = Iterator.iterate(1)(_*2)

  println(powersOf2.indexOf(8))

  // get the nth element of an iterator : .drop(n-1).next()
  println(powersOf2.drop(3).next())
}
