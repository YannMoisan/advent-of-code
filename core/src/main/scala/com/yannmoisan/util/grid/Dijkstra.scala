package com.yannmoisan.util.grid

import scala.collection.mutable

object Dijkstra {
  def dijkstra(grid: Grid[Int]) = {
    val dist = Array.fill[Int](grid.dim.size)(Int.MaxValue)
    dist(0) = 0
    val toVisit = mutable.TreeSet[(Int, Int)](dist.zipWithIndex.toIndexedSeq: _*)

    while (toVisit.size > 0) {
      // find min
      val (d, s1) = toVisit.head
      val _       = toVisit.remove((d, s1))
      grid.dim.neighbors4(s1).foreach { j =>
        if (dist(j) > dist(s1) + grid(j)) {
          val _ = toVisit.remove((dist(j), j))
          dist(j) = dist(s1) + grid(j)
          val _ = toVisit.addOne((dist(j), j))
        }
      }
    }
    dist(grid.dim.size - 1)
  }

}
