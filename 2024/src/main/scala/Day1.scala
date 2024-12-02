object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val (fst, snd) =
      input
        .map { s =>
          val Array(fst, snd) = s.split("   ")
          (fst.toInt, snd.toInt)
        }
        .toList
        .unzip
    fst.sorted.zip(snd.sorted).foldLeft(0) { case (sum, (fst, snd)) => sum + math.abs(fst - snd) }
  }

  override def part2(input: Iterator[String]): Int = {
    val (fst, snd) =
      input
        .map { s =>
          val Array(fst, snd) = s.split("   ")
          (fst.toInt, snd.toInt)
        }
        .toList
        .unzip
    val frequencies = snd.groupMapReduce(identity)(_ => 1)(_ + _)
    fst.foldLeft(0) { case (sum, e) => sum + e * frequencies.getOrElse(e, 0) }
  }
}
