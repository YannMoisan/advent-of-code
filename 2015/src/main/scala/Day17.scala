// combination
// Learning : combination doestn't work with duplicates
@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day17 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val containers = input.map(_.toInt).toArray

    val subsets: Iterator[Set[Int]] = (0 to 19).toSet.subsets()
    val comb: Iterator[List[Int]]   = subsets.map(indices => indices.toList.map(containers))
    comb.count(_.sum == 150)

//    val solutions: Seq[List[Int]] = for {
//      i  <- 1 to containers.length
//      ll <- containers.combinations(i)
//    } yield ll
//    solutions.count(_.sum == 150) //.take(10).toList)
  }
// 737 That's not the right answer; your answer is too low.
// 2142 too high
  override def part2(input: Iterator[String]): Int = {
    val containers = input.map(_.toInt).toArray

    val subsets: Iterator[Set[Int]] = (0 to 19).toSet.subsets()
    val comb: Iterator[List[Int]]   = subsets.map(indices => indices.toList.map(containers))
    val candidates: List[List[Int]] = comb.filter(_.sum == 150).toList
    val minLength                   = candidates.minBy(_.length).length
    candidates.count(_.length == minLength)

  }
}
