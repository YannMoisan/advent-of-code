import scala.collection.immutable.Seq

object Day16 extends SinglePuzzle[String, Int] {
  private val basePattern = List(0, 1, 0, -1)

  override def part1(input: String): String = {
    val signal: Seq[Int] = input.toCharArray.map(_.asDigit).toIndexedSeq
    Iterator.iterate(signal)(next).drop(100).next().mkString.substring(0, 8)
  }

  override def part2(input: String): Int = 42

  private def next(signal: Seq[Int]): Seq[Int] =
    signal.indices.map { index =>
      val sum =
        signal.zip(repeatingPattern(index + 1)).foldLeft(0) { case (acc, (a, b)) => acc + a * b }
      math.abs(sum % 10)
    }

  def repeatingPattern(i: Int): Iterator[Int] = {
    val l = List.fill(i)(basePattern(0)) ++
      List.fill(i)(basePattern(1)) ++
      List.fill(i)(basePattern(2)) ++
      List.fill(i)(basePattern(3))
    Iterator.continually(l).flatten.drop(1)
  }

}
