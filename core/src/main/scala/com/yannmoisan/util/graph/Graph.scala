package com.yannmoisan.util.graph

import scala.collection.mutable
// 0 -) 1 4 5
// 1 4
// 2 1
// 3 2 4
//

case class Node(name: String, neighbours: List[String])
case class Graph(nodes: List[Node])

// TODO all paths between A and B
// TODO all paths from 0 to 4

object GraphTraversal extends App {
  // Directed but not acyclic graph
  // Not a DAG 1 -> 3 -> 2 -> 1
  val g = Map(
    0 -> List(1, 4, 5),
    1 -> List(3, 4),
    2 -> List(1),
    3 -> List(2, 4)
  )

  // 1. java.lang.StackOverflowError
  def dfs(start: Int): Unit = {
    println(s"visited:$start")
    g.get(start).foreach(_.foreach(dfs))
  }

  // 2. global set of visited
  // Pb1 : not tail rec
  // Pb2 : mutable visited
  def dfs2(start: Int, visited: mutable.Set[Int]): Unit = {
    println(s"visited:$start")
    val _ = visited.add(start)
    g.get(start).foreach { n =>
      n.foreach { node =>
        if (!visited.contains(node)) {
          dfs2(node, visited)
        }
      }
    }
  }

  // 2. global set of visited
  // Pb1 : not tail rec
  // Doesnt work on the example, 4 is visited multiple times because visited is not updating when backtracking occurs.
  def dfs3(start: Int, visited: Set[Int]): Unit = {
    println(s"visited:$start")
    //val _ = visited.add(start)
    g.get(start).foreach { n =>
      n.foreach { node =>
        if (!visited.contains(node)) {
          dfs3(node, visited.+(start))
        }
      }
    }
  }

  // dfs without recursion
  // changement de l'ordre de parcours need .reverse
  // 4 is visited multiple times
  def dfs4(start: Int): Unit = {
    var stack   = List(start)
    val visited = mutable.Set[Int]()
    while (!stack.isEmpty) {
      val h :: t = stack
      stack = t
      println(s"visited:$h")
      val _ = visited.add(h)
      g.get(h).foreach { n =>
        n.reverse.foreach { node =>
          if (!visited.contains(node) && !stack.contains(node)) {
            stack = node :: stack
          }
        }
      }
    }
  }

  // 2. global set of visited
//  val visited2 = mutable.Set[Int]()
//  @tailrec
//  def dfs3(start: Int): Unit = {
//    println(s"visited:$start")
//    val _ = visited2.add(start)
//    g.get(start).foreach { n =>
//      n.foreach { node =>
//        if (!visited2.contains(node)) {
//          dfs3(node)
//        }
//      }
//    }
//  }

  //dfs4(0)
  //dfs3(0, Set[Int]())

  def bfs(start: Int): Unit = {
    val q       = mutable.Queue[Int]()
    val visited = mutable.Set[Int]()
    val _       = q.enqueue(start)
    while (!q.isEmpty) {
      val current = q.dequeue()
      println(s"visited:$current")
      val _ = visited.add(current)
      g.get(current).foreach { n =>
        n.foreach { node =>
          if (!visited.contains(node) && !q.contains(node)) {
            val _ = q.enqueue(node)
          }
        }
      }
    }
  }

  bfs(0)
}
