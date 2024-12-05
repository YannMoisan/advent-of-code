import scala.collection.immutable.{Map, Set}

object Day5 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val lines = input.toList
    val orderingRules = lines
      .take(1176)
      .map { case s"${fst}|${snd}" => (fst.toInt, snd.toInt) }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

    val updates: Seq[Array[Int]] = lines
      .drop(1177).toList
      .map(s => s.split(",").map(_.toInt))

    updates
      .filter(isSorted(orderingRules))
      .map(arr => arr(arr.length / 2)).sum
  }

  private def isSorted(orderingRules: Map[Int, Set[Int]])(arr: Array[Int]): Boolean =
    arr.sliding(2).forall(l => lt(orderingRules)(l(0), l(1)))

  private def lt(orderingRules: Map[Int, Set[Int]]): (Int, Int) => Boolean = { (a, b) =>
    orderingRules.get(a).exists(_.contains(b))
  }

  override def part2(input: Iterator[String]): Int = {
    val lines = input.toList
    val orderingRules: Map[Int, Set[Int]] = lines
      .take(1176)
      .map { case s"${fst}|${snd}" => (fst.toInt, snd.toInt) }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

    val updates: Seq[Array[Int]] = lines
      .drop(1177).toList
      .map(s => s.split(",").map(_.toInt))

    updates
      .filter(arr => !isSorted(orderingRules)(arr))
      .map(arr => arr.sortWith(lt(orderingRules)))
      .map(arr => arr(arr.length / 2)).sum
  }

}
