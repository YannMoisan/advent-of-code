object Day13 extends MultiPuzzle[Long, Int] {

  //Button A: X+79, Y+87
  //Button B: X+44, Y+14
  //Prize: X=7384, Y=4824

  // a1*x + b1 * y = c1
  // a2*x + b2 * y = c2
  override def part1(input: Iterator[String]): Long = {
    val tmp = input
      .grouped(4).map { l =>
        val s"Button A: X+${a1}, Y+${a2}" = l(0)
        val s"Button B: X+${b1}, Y+${b2}" = l(1)
        val s"Prize: X=${d1}, Y=${d2}"    = l(2)

        val c1: Long = d1.toInt + 10000000000000L
        val c2: Long = d2.toInt + 10000000000000L

        val discriminant = a1.toInt * b2.toInt - a2.toInt * b1.toInt
        val x: Double    = (c1 * b2.toInt - c2 * b1.toInt).toDouble / discriminant
        val y: Double    = (-c1 * a2.toInt + c2 * a1.toInt).toDouble / discriminant
        println(s"$x $y")

        if (x.toLong == x && y.toLong == y) {
          x * 3 + y
        } else
          0

//        println(s"$x $y")
//
//        for {
//          a <- 1 to 100
//          b <- 1 to 100
//          if a * a1.toInt + b * b1.toInt == c1.toInt && a * a2.toInt + b * b2.toInt == c2.toInt
//        } yield a * 3 + b

// 26299

//
//      val discriminant = a1.toInt * b2.toInt - a2.toInt * b1.toInt
//      val x            = (c1.toInt * b2.toInt - c2.toInt * b1.toInt).toDouble / discriminant
//      val y            = (-c1.toInt * a2.toInt + c2.toInt * a1.toInt).toDouble / discriminant
//
//      println(s"$x $y")
//
////      val index = (1 to 10000).find(i => x * i == (x * i).toInt && y * i == (y * i).toInt)
////      if (index.isDefined)
////        println(s"${x * index.get} ${y * index.get}")
//
//    //println(s"$x $y")
      }.toList
    tmp.foreach(println)

    tmp.sum.toLong
  }

  override def part2(input: Iterator[String]): Int = ???
}
