import scala.collection.mutable

object Day6 extends MultiPuzzle[Int, Int] {
  // it's a tree, each node has only one parent
  // sum of height in the tree
  override def part1(lines: Iterator[String]): Int = {
    val adj: Seq[(String, String)] = lines.map { line =>
      val Array(src, dst) = line.split(')')
      (src, dst)
    }.toSeq

    val m = mutable.Map("COM" -> 0)
    dfsLength("COM", adj, m)
    m.values.sum
  }

  override def part2(lines: Iterator[String]): Int = {
    val adj: Seq[(String, String)] = lines.map { line =>
      val Array(src, dst) = line.split(')')
      (src, dst)
    }.toSeq

    val m = mutable.Map("COM" -> Vector("COM"))
    dfsPath("COM", adj, m)
    val path1 = m("YOU")
    val path2 = m("SAN")

    val common = path1.intersect(path2)
    path1.length - (common.length + 1) + path2.length - (common.length + 1)
  }

  private def dfsLength(
      node: String,
      adj: Seq[(String, String)],
      acc: mutable.Map[String, Int]
  ): Unit =
    adj.filter(_._1 == node).foreach { tuple =>
      acc.put(tuple._2, acc(node) + 1)
      dfsLength(tuple._2, adj, acc)
    }

  private def dfsPath(
      node: String,
      adj: Seq[(String, String)],
      acc: mutable.Map[String, Vector[String]]
  ): Unit =
    adj.filter(_._1 == node).foreach { tuple =>
      acc.put(tuple._2, acc(node) :+ tuple._2)
      dfsPath(tuple._2, adj, acc)
    }
}
