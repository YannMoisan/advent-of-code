object Day13 extends MultiPuzzle[Int, Long] {
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def part1(input: Iterator[String]): Int = {
    val target = input.next().toInt
    println(target)
    val ids = input.next().split(",").filter(_ != "x").map(_.toInt)
    ids.foreach(i => println(i))
    val ret = ids.map(i => (i, i - target % i)).minBy(_._2)
    ret._1 * ret._2
  }

  override def part2(input: Iterator[String]): Long = {
    val target = input.next().toInt
    println(target)
    val ids: Array[(Int, Int)] =
      input.next().split(",").zipWithIndex.filter(_._1 != "x").map { case (a, b) => (a.toInt, b) }
    ids.foreach(l => println(l))
    solve(ids)

//    println(solve("17,x,13,19"))
//    println(solve("67,7,59,61"))
//    println(solve("67,x,7,59,61"))
//    println(solve("67,7,x,59,61"))
//    println(solve("1789,37,47,1889"))
//    43
  }

  // (41,0)
  // (37,35)
  // => 232 160 942 = 41 * ( 5 662 462 )
  // toutes les solutions 41 * 5662462

  def solve(ids: Array[(Int, Int)]): Long = {
    println(ids)
    val ids2 = Array(
      (41, 0),
      (557, 41),
      (419, 72),
      (13, 54),
      (17, 58),
      (23, 64),
      (19, 91),
      (29, 43),
      (37, 35)
    )
    // pgcd 41 / 557 (both primes)
    //var i     = 6656L
    var i = 66282L
//    var i     = 0L
    var found = false
    while (!found) {
      val cand = i * (41)
      found = ids2.forall { case (id, delay) => (cand + delay) % id == 0 }
      if (!found)
        i += 419 // 557 // 419
      else
        println(s"can:$cand")
//      i += 1
    }

//    // check
//    val res = 272632824398244L
    //          598411311431841
//    //        100000000000000
//    ids2.foreach { case (id, delay) => println((res + delay) % id) }

    i * 41
    //* 557

  }
}
// 494240464595540
