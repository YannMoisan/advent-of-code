import scala.collection.immutable.{Map, Set}
// string interpolation to destructure string
// perte de temps à cause de i instead of arr(i) et ça compile

object Day5 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val lines = input.toList
    val orderingRules: Map[Int, Set[Int]] = lines
      .take(1176)
      .map { case s"${fst}|${snd}" => (fst.toInt, snd.toInt) }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet).toMap

    val updates: Seq[Array[Int]] = lines
      .drop(1177).toList
      .map(s => s.split(",").map(_.toInt))

    val res = updates.filter(isOrdered(orderingRules, _))

    res.map(arr => arr(arr.length / 2)).sum
  }

  private def isOrdered(rules: Map[Int, Set[Int]], arr: Array[Int]) =
    arr.indices.forall { i =>
      (0 until i).forall(j => !rules.getOrElse(arr(i), Set()).contains(arr(j)))
    }

  private def unordererPairs(rules: Map[Int, Set[Int]], arr: Array[Int]) =
    for {
      i <- arr.indices
      j <- (0 until i)
      if rules.getOrElse(arr(i), Set()).contains(arr(j))
    } yield (i, j)

  // TODO use sortBy
  private def sort(rules: Map[Int, Set[Int]], arr: Array[Int]): Array[Int] = {
    while (!isOrdered(rules, arr)) {
      val (i, j) = unordererPairs(rules, arr).head
      // swap values
      val tmp = arr(j)
      arr(j) = arr(i)
      arr(i) = tmp
    }
    arr
  }

  override def part2(input: Iterator[String]): Int = {
    val lines = input.toList
    val orderingRules: Map[Int, Set[Int]] = lines
      .take(1176)
      .map { case s"${fst}|${snd}" => (fst.toInt, snd.toInt) }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet).toMap

    val updates: Seq[Array[Int]] = lines
      .drop(1177).toList
      .map(s => s.split(",").map(_.toInt))

    val res = updates.filter(!isOrdered(orderingRules, _))

    res
      .map(arr => sort(orderingRules, arr))
      .map(arr => arr(arr.length / 2)).sum

  }
}
