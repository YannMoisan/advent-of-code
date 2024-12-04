object Day16 extends MultiPuzzle[Int, Int] {
  private val scam = Map(
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

  override def part1(input: Iterator[String]): Int =
    common(
      input,
      { case (item, qty) =>
        item match {
//        case "cats" | "trees" => scam(item) < qty
//        case "pomeranians" | "goldfish" => scam(item) > qty
          case _ => scam(item) == qty

        }
      }
    )

  override def part2(input: Iterator[String]): Int =
    common(
      input,
      { case (item, qty) =>
        item match {
          case "cats" | "trees"           => scam(item) < qty
          case "pomeranians" | "goldfish" => scam(item) > qty
          case _                          => scam(item) == qty

        }
      }
    )

  private def common(input: Iterator[String], p: ((String, Int)) => Boolean): Int = {
    val sues: Seq[(String, Seq[(String, Int)])] = input.map { line =>
      val s"Sue $i: $item1: $qty1, $item2: $qty2, $item3: $qty3" = line
      (i, Seq(item1 -> qty1.toInt, item2 -> qty2.toInt, item3 -> qty3.toInt))
    }.toList
    val candidates = sues.filter(sue => sue._2.forall(p))
    candidates.head._1.toInt
  }
}

// In particular, the cats and trees readings indicates that there are greater than that many
// (due to the unpredictable nuclear decay of cat dander and tree pollen),
// while the pomeranians and goldfish readings indicate that there are fewer than that many
// (due to the modial interaction of magnetoreluctance).
