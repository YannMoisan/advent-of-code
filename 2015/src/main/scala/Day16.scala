object Day16 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val sues = input.map { line =>
      val s"Sue $i: $item1: $qty1, $item2: $qty2, $item3: $qty3" = line
      (i, Seq(item1 -> qty1.toInt, item2 -> qty2.toInt, item3 -> qty3.toInt))
    }.toList
    val scam = Map(
      "children"    -> 3,
      "cats"        -> 7,
      "samoyeds"    -> 2,
      "pomeranians" -> 3,
      "akitas"      -> 0,
      "vizslas"     -> 0,
      "goldfish"    -> 5,
      "trees"       -> 3,
      "cars"        -> 2,
      "perfumes"    -> 1
    )
    val candidates = sues.filter(sue =>
      scam(sue._2(0)._1) == sue._2(0)._2 && scam(sue._2(1)._1) == sue
        ._2(1)._2 && scam(sue._2(2)._1) == sue._2(2)._2
    )
    println(candidates)
    42
  }

  override def part2(input: Iterator[String]): Int = {
    val sues = input.map { line =>
      val s"Sue $i: $item1: $qty1, $item2: $qty2, $item3: $qty3" = line
      (i, Seq(item1 -> qty1.toInt, item2 -> qty2.toInt, item3 -> qty3.toInt))
    }.toList
    val scam = Map(
      "children"    -> 3,
      "cats"        -> 7,
      "samoyeds"    -> 2,
      "pomeranians" -> 3,
      "akitas"      -> 0,
      "vizslas"     -> 0,
      "goldfish"    -> 5,
      "trees"       -> 3,
      "cars"        -> 2,
      "perfumes"    -> 1
    )
    val candidates = sues.filter(sue =>
      sue._2.forall {
        case (item, qty) =>
          item match {
            case "cats" | "trees"           => scam(item) < qty
            case "pomeranians" | "goldfish" => scam(item) > qty
            case _                          => scam(item) == qty

          }
      }
    )
    println(candidates)
    42
  }
}

// In particular, the cats and trees readings indicates that there are greater than that many
// (due to the unpredictable nuclear decay of cat dander and tree pollen),
// while the pomeranians and goldfish readings indicate that there are fewer than that many
// (due to the modial interaction of magnetoreluctance).
