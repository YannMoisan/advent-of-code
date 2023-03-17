import scala.collection.Iterator

// 9 and 13 are similar
@SuppressWarnings(
  Array(
    "org.wartremover.warts.TraversableOps",
    "org.wartremover.warts.Throw",
    "org.wartremover.warts.OptionPartial"
  )
)
object Day9 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = allDistances(input).min

  override def part2(input: Iterator[String]): Int = allDistances(input).max

  private def allDistances(input: Iterator[String]): Iterator[Int] = {
    val distances = input.map { l =>
      val s"$from to $to = $dist" = l
      (from, to, dist.toInt)
    }.toList

    val locations = List(
      "AlphaCentauri",
      "Arbre",
      "Faerun",
      "Norrath",
      "Snowdin",
      "Straylight",
      "Tambi",
      "Tristram"
    )

    locations.permutations.map { visit =>
      val tmp = (visit.sliding(2).toList) map {
        case List(a, b) =>
          distances.find(d => (d._1 == a && d._2 == b) || (d._2 == a && d._1 == b)) match {
            case Some(dist) => dist._3
            case None       => throw new IllegalStateException(s"$a => $b")
          }
        case _ => -1
      }
      tmp.sum
    }
  }
}
