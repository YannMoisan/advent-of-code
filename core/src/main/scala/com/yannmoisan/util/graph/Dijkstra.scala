package com.yannmoisan.util.graph

import scala.collection.mutable

object Dijkstra {
  def dijkstra[A: Ordering](start: A, end: A, nodes: Seq[A], grid: A => Seq[(A, Int)]) = {
    val dist = mutable.Map[A, Int]()
    dist(start) = 0
    nodes.filter(_ != start).foreach(a => dist(a) = Int.MaxValue)

    val toVisit = mutable.TreeSet[(Int, A)]((0, start))
    while (toVisit.size > 0) {
      // find min
      val (d, s1) = toVisit.head
      val _       = toVisit.remove((d, s1))
      grid(s1).foreach { case (j, d2) =>
        if (dist(j) > dist(s1) + d2) {
          val _ = toVisit.remove((dist(j), j))
          dist(j) = dist(s1) + d2
          val _ = toVisit.addOne((dist(j), j))
        }
      }
    }
    dist(end)
  }

}
