import scala.collection.mutable.ArrayBuffer

object Day14 extends SinglePuzzle[String, Int] {
  override def part1(input: String): String = {
    val count = input.toInt
    val start = ArrayBuffer(3, 7)
    Iterator
      .iterate((0, 1))(next(start, _)).dropWhile(_ => start.size <= count + 10).next()
    start.slice(count, count + 10).mkString
  }

  override def part2(input: String): Int = {
    val start = ArrayBuffer(3, 7)
    Iterator
      .iterate((0, 1))(next(start, _)).zipWithIndex.dropWhile(_ =>
        !start.slice(start.length - 7, start.length).mkString.contains(input)
      ).next()
    start.length - input.length - 1
  }

  private def next(buf: ArrayBuffer[Int], p: (Int, Int)): (Int, Int) = {
    val sum    = buf(p._1) + buf(p._2)
    val strSum = sum.toString
    val digits = strSum.map(_.asDigit)
    digits.foreach(buf.addOne(_))
    val newP1 = (1 + p._1 + buf(p._1)) % buf.length
    val newP2 = (1 + p._2 + buf(p._2)) % buf.length
    (newP1, newP2)
  }
}
