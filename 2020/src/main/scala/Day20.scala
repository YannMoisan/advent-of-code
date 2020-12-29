@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day20 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val tiles: Seq[(String, Array[String])] = input
      .grouped(12).map { lines =>
        val s"Tile $id:" = lines.head
        (id, lines.tail.toArray)
      }.toList
    val tmp3: Seq[(String, Seq[String])] = tiles.map { case (id, grid)    => (id, borderValues(grid)) }
    val tmp: Seq[String]                 = tiles.flatMap { case (_, grid) => borderValues(grid) }
    val uniqueBorders: Set[String] = tmp
      .groupBy(identity).map { case (k, vals) => (k, vals.size) }.filter(_._2 == 1).map(_._1).toSet

    val tmp4: Seq[String] = tmp3.collect {
      case (id, values) if values.count(uniqueBorders.contains) == 4 => id
    }
    println(tmp4.length)
    println(tmp4.map(_.toLong).product)

    println(tiles.length)
    //take(2).foreach(l => println(l))
    42
  }

  def borderValues(g: Array[String]): Seq[String] = {
    val a = (0 to 9).map(i => g(i)(0)).mkString
    val b = (0 to 9).map(i => g(i)(9)).mkString
    val c = (0 to 9).map(i => g(0)(i)).mkString
    val d = (0 to 9).map(i => g(9)(i)).mkString
    Seq(
      a,
      a.reverse,
      b,
      b.reverse,
      c,
      c.reverse,
      d,
      d.reverse
    )
  }

  override def part2(input: Iterator[String]): Int = 43
}
