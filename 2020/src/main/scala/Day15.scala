import scala.collection.mutable

object Day15 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int =
    run(input, 2020)

  override def part2(input: String): Int =
    run(input, 30000000)

  def run(input: String, limit: Int) = {
    val values: Array[Int] = input.split(",").map(_.toInt)
    var i                  = 1
    val mem                = mutable.Map[Int, Int]()
    var lastNumberSpoken   = -1
    while (i <= limit) {
      if (i > 1 && i <= values.length) {
        mem(lastNumberSpoken) = i - 1
      }
      if (i <= values.length) {
        lastNumberSpoken = values(i - 1)
      } else {
        mem.get(lastNumberSpoken) match {
          case Some(turn) =>
            mem(lastNumberSpoken) = i - 1
            lastNumberSpoken = i - 1 - turn
          case None =>
            mem(lastNumberSpoken) = i - 1
            lastNumberSpoken = 0
        }
      }
      i += 1
    }
    lastNumberSpoken
  }
}
