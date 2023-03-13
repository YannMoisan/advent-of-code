package com.yannmoisan.util.grid

import scala.collection.mutable

object BFS {
  def shortestPath(
                    grid: Grid[Char],
                    start: Pos,
                    target: Char,
                    isValid: (Pos, Pos) => Boolean
  ): Option[List[Pos]] = {
    val q = mutable.Queue[List[Pos]]()
    val visited = Array.ofDim[Boolean](grid.dim.width * grid.dim.height)
    q.enqueue(List(start))
    var targetedPath: Option[List[Pos]] = None
    while (q.nonEmpty && targetedPath.isEmpty) {
      val path = q.dequeue()
      val pos = path.head
      if (grid(pos) == target) {
        targetedPath = Some(path.reverse)
      } else {
        val arr = grid.dim.neighbors(pos)
        // PERF: while loop is faster than array.foreach
        var i = 0
        while (i < arr.length) {
          val newPos = arr(i)
          if (
            !visited(
              newPos.index
            ) && isValid(pos, newPos)
          ) {
            q.enqueue(newPos :: path)
            visited(newPos.index) = true
          }
          i += 1
        }
      }
    }
    targetedPath
  }
}
