// brute force ?
@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val tmp = for {
      a <- 0 to 100
      b <- 0 to 100 - a
      c <- 0 to 100 - (a + b)
      d = 100 - (a + b + c)
    } yield {
      val aa = math.max(0, 2 * a)
      val bb = math.max(0, 5 * b - d)
      val cc = math.max(0, -2 * a - 3 * b + 5 * c)
      val dd = math.max(0, -c + 5 * d)
      aa * bb * cc * dd
    }
    tmp.max
  }

  override def part2(input: Iterator[String]): Int = {
    val tmp = for {
      a <- 0 to 100
      b <- 0 to 100 - a
      c <- 0 to 100 - (a + b)
      d = 100 - (a + b + c)
      if 3 * a + 3 * b + 8 * c + 8 * d == 500
    } yield {
      val aa = math.max(0, 2 * a)
      val bb = math.max(0, 5 * b - d)
      val cc = math.max(0, -2 * a - 3 * b + 5 * c)
      val dd = math.max(0, -c + 5 * d)
      aa * bb * cc * dd
    }
    tmp.max
  }
}
