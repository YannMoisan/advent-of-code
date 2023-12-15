import scala.collection.mutable

object Day15 extends SinglePuzzle[Int, Int] {
  private def hash(s: String): Int =
    s.foldLeft(0) { case (c, v) => ((v + c.toInt) * 17) % 256 }

  override def part1(input: String): Int = {
    println(hash("HASH"))
    input.split(',').map(hash).sum
  }

  override def part2(input: String): Int = {
    val arr = Array.fill(256)(mutable.LinkedHashMap[String, Int]())
    input.split(',').collect {
      case s"$h-"   => arr(hash(h)).remove(h)
      case s"$h=$v" => arr(hash(h)).update(h, v.toInt)
    }
    arr.indices.map { arrIndex =>
      arr(arrIndex).zipWithIndex.map { case ((_, v), index) => (arrIndex + 1) * v * (index + 1) }.sum
    }.sum
  }
}
