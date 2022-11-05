import scala.collection.mutable

object Day7 extends MultiPuzzle[Int, Int] {
  // Find all distinct ancestors of a vertex in a DAG (repr. as adjacency list)
  override def part1(input: Iterator[String]): Int = {
    val rules: Seq[(String, List[(Int, String)])] = input.map(parse).toList
    val q: mutable.Queue[String]                  = new mutable.Queue[String]()
    val visited                                   = mutable.Set[String]()
    val _                                         = q.enqueue("shiny gold")
    while (!q.isEmpty) {
      val cur = q.dequeue()
      rules.foreach {
        case (outer, inners) =>
          if (inners.exists(_._2 == cur)) {
            val _ = visited.add(outer)
            val _ = q.enqueue(outer)
          }
      }
    }
    visited.size
  }

  // Traverse all child vertices of a vertex in a DAG (repr. as adjacency list)
  // The impl. is a non tail rec DFS
  override def part2(input: Iterator[String]): Int = {
    val rules: Seq[(String, List[(Int, String)])] = input.map(parse).toList
    cost(rules, "shiny gold")
  }

  def cost(rules: Seq[(String, List[(Int, String)])], s: String): Int =
    rules.find(_._1 == s) match {
      case Some(l) => l._2.map { case (qty, color) => qty * (1 + cost(rules, color)) }.sum
      case None    => sys.error("illegal state")
    }

  // muted lavender bags contain 5 dull brown bags, 4 pale maroon bags, 2 drab orange bags.
  def parse(s: String): (String, List[(Int, String)]) =
    s match {
      case s"$outer bags contain no other bags." => (outer, List())
      case s"$outer bags contain $inners." =>
        val regex = raw"(\d+) ((?:\w+ ?)+) bag".r
        val bags  = regex.findAllMatchIn(inners).toList.map(m => (m.group(1).toInt, m.group(2)))
        (outer, bags)
    }
}
