package com.yannmoisan.aoc

import scala.collection.immutable.Queue

object Day12 extends MultiPuzzle[Int, Int] {
  object BFS {

    /**
      * Breadth first traversal of a graph defined by a function `A => Seq[A]`.
      * Each node is visited at most one time.
      *
      * @param node
      * @param f definition of the graph
      * @tparam Node
      * @return a stream of tuples : the current node and its ancestors
      */
    def breadth_first_traverse[Node](
        node: Node,
        f: Node => Seq[Node]): Stream[(Node, Seq[Node])] = {
      def recurse(q: Queue[(Node, Seq[Node])],
                  cache: Set[Node]): Stream[(Node, Seq[Node])] = {
        if (q.isEmpty) {
          Stream.Empty
        } else {
          val ((node, i), tail) = q.dequeue
          val nodes = f(node).filterNot(cache.contains)
          (node, i) #:: recurse(tail ++ nodes.map(n => (n, n +: i)),
                                cache ++ nodes)
        }
      }

      (node, Seq(node)) #:: recurse(
        Queue.empty[(Node, Seq[Node])] ++ f(node).map(n => (n, Seq(n, node))),
        Set.empty)
    }
  }

  override def part1 = { lines =>
    val arr = lines
      .map(_.split(" <-> "))
      .map { case Array(a, b) => (a, b.split(", ").toSeq) }
    val m: Map[String, Seq[String]] = arr.toMap
    val stream = BFS.breadth_first_traverse(
      "0",
      (s: String) => m.getOrElse(s, Seq.empty[String]))
    stream.take(10).foreach(println(_))
    0
  }

  override def part2 = { lines =>
    0
  }
}
