import scala.collection.mutable.ArrayBuffer

object Day17 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    val buf = ArrayBuffer(0)
    val _   = Iterator.iterate((0, 1))(t => update(buf, t._1, t._2, 382)).drop(2017).next()
    buf(buf.indexOf(2017) + 1)
  }

  // return the new current position
  private def update(buf: ArrayBuffer[Int], cur: Int, value: Int, step: Int): (Int, Int) = {
    val insertAt = (cur + step) % buf.size + 1
    buf.insert(insertAt, value)
    (insertAt, value + 1)
  }

  override def part2(input: String): Int = {
    val buf = ArrayBuffer(0)
    val _   = Iterator.iterate((0, 1))(t => update(buf, t._1, t._2, 382)).drop(50_000_000).next()
    buf(buf.indexOf(0) + 1)
  }
}
