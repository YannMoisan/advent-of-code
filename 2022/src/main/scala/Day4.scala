object Day4 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input.map { s =>
      val s"$fst1-$fst2,$snd1-$snd2" = s
      if (fst1.toInt <= snd1.toInt && fst2.toInt >= snd2.toInt) 1
      else if (fst2.toInt <= snd2.toInt && fst1.toInt >= snd1.toInt) 1
      else 0
    }.sum

  override def part2(input: Iterator[String]): Int =
    input.map { s =>
      val s"$fst1-$fst2,$snd1-$snd2" = s
      if (fst1.toInt >= snd1.toInt && fst1.toInt <= snd2.toInt) 1
      else if (fst2.toInt >= snd1.toInt && fst2.toInt <= snd2.toInt) 1
      else if (fst1.toInt <= snd1.toInt && fst2.toInt >= snd2.toInt) 1
      else if (fst2.toInt <= snd2.toInt && fst1.toInt >= snd1.toInt) 1
      else 0
    }.sum

}
