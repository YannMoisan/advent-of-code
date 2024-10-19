object Day12 extends MultiPuzzle[Int, Int] {
  def count(line: String) = {
    var c              = 0
    var res: List[Int] = List()
    line.foreach { i =>
      if (i == '#') c += 1
      else {
        if (c > 0)
          res = res :+ c
        c = 0
      }
    }
    if (c > 0)
      res = res :+ c
    res
  }

//  def all(s: String, cur: List[String]) : List[String] = {
//    val i = s.indexOf('?')
//    if (i == -1) line else
//      Seq(line.updated(i, '.'), line.updated(i, '#')
//
//
//        line.indexOf('?')
//  }

  override def part1(input: Iterator[String]): Int = {
//

    println(count("#.#.###"))             //1,1,3
    println(count(".#...#....###."))      //1,1,3
    println(count(".#.###.#.######"))     //1,3,1,6
    println(count("####.#...#..."))       //4,1,1
    println(count("#....######..#####.")) //1,6,5
    println(count(".###.##....#"))        //3,2,1

    42
  }

  override def part2(input: Iterator[String]): Int = 43
}
