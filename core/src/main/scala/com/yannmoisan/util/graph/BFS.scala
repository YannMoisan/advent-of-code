package com.yannmoisan.util.graph

import scala.collection.immutable.Queue
import scala.collection.mutable

object BFS {

  /**
    * Breadth first traversal of a graph defined by a function `A => Seq[A]`.
    * Each node is visited at most one time.
    *
    * @param node
    * @param f definition of the graph
    * @tparam A
    * @return a stream of tuples : the current node and its ancestors
    */
  def breadth_first_traverse[A](
      node: A,
      f: A => Seq[A]
  ): LazyList[(A, Seq[A])] = {
    def recurse(q: Queue[(A, Seq[A])], cache: Set[A]): LazyList[(A, Seq[A])] =
      if (q.isEmpty) {
        LazyList.empty
      } else {
        val ((node, i), tail) = q.dequeue
        val nodes             = f(node).filterNot(cache.contains)
        (node, i) #:: recurse(tail ++ nodes.map(n => (n, n +: i)), cache ++ nodes)
      }

    (node, Seq(node)) #:: recurse(Queue.empty ++ f(node).map(n => (n, Seq(n, node))), Set.empty)
  }

  def breadth_first_traverse_no_path_it[A](
      node: A,
      f: A => Seq[A]
  ): Iterator[A] =
    new Iterator[A] {
      val queue   = mutable.Queue[A]()
      val visited = mutable.Set[A]()
      queue.enqueue(node)
      visited.add(node)

      override def hasNext: Boolean = !queue.isEmpty

      override def next(): A = {
        val cur            = queue.dequeue()
        val nextNotVisited = f(cur).filter(!visited.contains(_))
        queue.enqueueAll(nextNotVisited)
        visited.addAll(nextNotVisited)
        cur
      }
    }

  def main(args: Array[String]): Unit = {
    val init         = 0
    def next(i: Int) = List(i - 1, i + 1)

    val stream = breadth_first_traverse(init, next)
    val os     = stream.find(_._1 == 10)
    println(os)
  }

}
