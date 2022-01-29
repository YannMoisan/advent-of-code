@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day20 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val lines = input.toArray
//    val algorithm =
//      "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#" /*lines(0)*/
    val algorithm = lines(0)
    val image     = lines.drop(2)
    println(s"size=${image.head.length}/${image.size}")

//    val img = Array(
//      "...............",
//      "...............",
//      "...............",
//      "...............",
//      "...............",
//      ".....#..#......",
//      ".....#.........",
//      ".....##..#.....",
//      ".......#.......",
//      ".......###.....",
//      "...............",
//      "...............",
//      "...............",
//      "...............",
//      "..............."
//    )
//52: 19125
//53: 19671
//54: 19818
//55: 19991
    val withBorder = addBorder(image, 200) //.foreach(arr => println(arr.mkString))

//    println(value(Array("...", "#..", ".#."), 1, 1))

    var end = withBorder
    (0 until 50).foreach(_ => end = next(algorithm)(end))

//    val step1 = next(algorithm)(withBorder)
//    val end   = next(algorithm)(step1)

    //val end = Iterator.iterate(withBorder)(next(algorithm)).take(1).toArray.last
    //println(s"size=${end.head.length}/${end.size}")

    end.foreach(r => println(r))

    //4968

    (for {
      x <- 49 until end.head.length - 49
      y <- 49 until end.length - 49
    } yield { if (end(y)(x) == '#') 1 else 0 }).sum
  }

  override def part2(input: Iterator[String]): Int = 42

  def next(algorithm: String)(grid: Array[String]): Array[String] =
    Array
      .tabulate(grid.head.length, grid.length) {
        case (y, x) =>
          algorithm(value(grid, x, y))
      }.map(_.mkString)

  def value(g: Array[String], x: Int, y: Int) = {
    val bits = for {
      j <- -1 to 1
      i <- -1 to 1
      ny = y + j
      nx = x + i
      if nx >= 0 && nx < g.head.length && ny >= 0 && ny < g.length
    } yield { if (g(ny)(nx) == '#') '1' else '0' }
    Integer.parseInt(bits.mkString, 2)
  }

  def addBorder(g: Array[String], size: Int): Array[String] = {
    val newWidth  = g.head.length + 2 * size
    val newHeight = g.size + 2 * size
    Array
      .tabulate(newHeight, newWidth) {
        case (y, x) =>
          if (x < size || x >= g.head.length + size || y < size || y >= g.size + size) {
            '.'
          } else {
            g(y - size)(x - size)
          }
      }.map(_.mkString)
  }
}
