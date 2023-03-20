import scala.collection.immutable.Queue

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
  def breadth_first_traverse[Node](node: Node, f: Node => Seq[Node]): Stream[(Node, Seq[Node])] = {
    def recurse(q: Queue[(Node, Seq[Node])], cache: Set[Node]): Stream[(Node, Seq[Node])] =
      if (q.isEmpty) {
        Stream.Empty
      } else {
        val ((node, i), tail) = q.dequeue
        val nodes             = f(node).filterNot(cache.contains)
        (node, i) #:: recurse(tail ++ nodes.map(n => (n, n +: i)), cache ++ nodes)
      }

    (node, Seq(node)) #:: recurse(Queue.empty ++ f(node).map(n => (n, Seq(n, node))), Set.empty)
  }

  def main(args: Array[String]): Unit = {
    val init         = 0
    def next(i: Int) = List(i - 1, i + 1)
    def pred(i: Int) = i == 10

    val stream = breadth_first_traverse(init, next)
    val os     = stream.find(_._1 == 10)
    println(os)
  }

}
