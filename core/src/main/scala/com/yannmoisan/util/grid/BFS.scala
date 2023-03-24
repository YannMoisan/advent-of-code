package com.yannmoisan.util.grid

import scala.collection.mutable

object BFS {
  def shortestPath(
      grid: Grid[Char],
      start: Int,
      target: Char,
      isValid: (Int, Int) => Boolean
  ): Option[List[Int]] = {
    val q       = mutable.Queue[List[Int]]()
    val visited = Array.ofDim[Boolean](grid.dim.width * grid.dim.height)
    q.enqueue(start :: Nil)
    var targetedPath: Option[List[Int]] = None
    while (q.nonEmpty && targetedPath.isEmpty) {
      val path = q.dequeue()
      val pos  = path.head
      if (grid(pos) == target) {
        targetedPath = Some(path.reverse)
      } else {
        val arr = grid.dim.neighbors4(pos)
        // PERF: while loop is faster than array.foreach
        var i = 0
        while (i < arr.length) {
          val newPos = arr(i)
          if (!visited(newPos) && isValid(pos, newPos)) {
            q.enqueue(newPos :: path)
            visited(newPos) = true
          }
          i += 1
        }
      }
    }
    targetedPath
  }

  def floodFill[A](g: Grid[A], isValid: (A, A) => Boolean, start: Int): mutable.Set[Int] = {
    val q       = mutable.Queue[Int]()
    val visited = mutable.Set[Int]()
    val _       = q.enqueue(start)
    val _       = visited.add(start)
    while (!q.isEmpty) {
      val i = q.dequeue()
      g.dim.neighbors4(i).foreach { j =>
        if (isValid(g(i), g(j)) && !visited.contains(j)) {
          val _ = q.enqueue(j)
          val _ = visited.add(j)
        }
      }
    }
    visited
  }

  def shortestPathTorus(
      grid: Grid[Char],
      start: Int,
      target: Char,
      isValid: (Int, Int) => Boolean
  ): Option[List[Int]] = {
    val q       = mutable.Queue[List[Int]]()
    val visited = Array.ofDim[Boolean](grid.dim.width * grid.dim.height)
    q.enqueue(start :: Nil)
    var targetedPath: Option[List[Int]] = None
    while (q.nonEmpty && targetedPath.isEmpty) {
      val path = q.dequeue()
      val pos  = path.head
      if (grid(pos) == target) {
        targetedPath = Some(path.reverse)
      } else {
        val arr = grid.dim.neighbors4Torus(pos)
        // PERF: while loop is faster than array.foreach
        var i = 0
        while (i < arr.length) {
          val newPos = arr(i)
          if (!visited(newPos) && isValid(pos, newPos)) {
            q.enqueue(newPos :: path)
            visited(newPos) = true
          }
          i += 1
        }
      }
    }
    targetedPath
  }
}
