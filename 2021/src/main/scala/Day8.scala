@SuppressWarnings(
  Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial")
)
object Day8 extends MultiPuzzle[Int, Int] {
  val sevenDigits = Map(
    Set('a', 'b', 'c', 'e', 'f', 'g')      -> '0',
    Set('c', 'f')                          -> '1',
    Set('a', 'c', 'd', 'e', 'g')           -> '2',
    Set('a', 'c', 'd', 'f', 'g')           -> '3',
    Set('b', 'c', 'd', 'f')                -> '4',
    Set('a', 'b', 'd', 'f', 'g')           -> '5',
    Set('a', 'b', 'd', 'e', 'f', 'g')      -> '6',
    Set('a', 'c', 'f')                     -> '7',
    Set('a', 'b', 'c', 'd', 'e', 'f', 'g') -> '8',
    Set('a', 'b', 'c', 'd', 'f', 'g')      -> '9'
  )
  override def part1(input: Iterator[String]): Int =
    input.map(parseLine).map(_._2.count(p => Seq(2, 4, 3, 7).contains(p.length))).sum

  // brute force : 5040
  override def part2(input: Iterator[String]): Int =
    input
      .map(parseLine).map {
        case (signals, values) =>
          val mappi = mapping(signals)
          decode2(values, mappi)
      }.sum

  private def parseLine(line: String): (Array[String], Array[String]) = {
    val Array(signals, value) = line.split(" \\| ") //.split(" | ") doesn't work as expected
    (signals.split(" "), value.split(" "))
  }

  private def mapping(signals: Array[String]): Map[Char, Char] = {
    val countBySegmentInAllDigits =
      signals.mkString.groupBy(identity).view.mapValues(_.length).toMap

    val e: Char = countBySegmentInAllDigits.find(_._2 == 4).get._1
    val b: Char = countBySegmentInAllDigits.find(_._2 == 6).get._1
    val f: Char = countBySegmentInAllDigits.find(_._2 == 9).get._1

    val c: Char = signals.find(_.length == 2).get.toSet.-(f).head
    val a: Char = signals.find(_.length == 3).get.toSet.removedAll(Set(c, f)).head
    val d: Char = signals.find(_.length == 4).get.toSet.removedAll(Set(b, c, f)).head
    val g: Char = Set('a', 'b', 'c', 'd', 'e', 'f', 'g').removedAll(Set(e, b, f, c, a, d)).head

    Map(
      a -> 'a',
      b -> 'b',
      c -> 'c',
      d -> 'd',
      e -> 'e',
      f -> 'f',
      g -> 'g'
    )

    // HashMap(e -> 8, f -> 7, a -> 7, b -> 8, g -> 9, c -> 6, d -> 4)

    // nb of occurrences in all 10 digits
    // e 4
    // b 6
    // d 7
    // g 7
    // a 8
    // c 8
    // f 9

    // => on connait e,f,b

    // taille 2 => on connait c
    // taille 3 => on connait a
    // taille 4 => on connait d

    // il reste que g
  }

  private def decode(str: String, mapping: Map[Char, Char]): Char =
    sevenDigits(str.map(mapping).toSet)

  private def decode2(str: Array[String], mapping: Map[Char, Char]): Int =
    Integer.parseInt(str.map(s => decode(s, mapping)).mkString)

}
