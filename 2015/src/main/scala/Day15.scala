// brute force ?
@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val tmp = for {
      a <- 0 to 100
      b <- 0 to 100
      c <- 0 to 100
      d <- 0 to 100
      if a + b + c + d == 100
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
      b <- 0 to 100
      c <- 0 to 100
      d <- 0 to 100
      if a + b + c + d == 100
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
