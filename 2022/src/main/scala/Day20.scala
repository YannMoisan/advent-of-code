import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object Day20 extends MultiPuzzle[Int, Long] {
  override def part1(input: Iterator[String]): Int = {
    val inputs = input.map(_.toInt).toArray.zipWithIndex

    val buf: ArrayBuffer[(Int, Int)] = ArrayBuffer(ArraySeq.unsafeWrapArray(inputs): _*)
    inputs.foreach {
      case element @ (value, _) =>
        val prevIndex = buf.indexOf(element)
        val newIndex  = math.floorMod(prevIndex + value, inputs.size - 1)
        buf.remove(prevIndex)
        buf.insert(newIndex, element)
    }

    val buf2 = buf.map(_._1)

    val index0 = buf2.indexOf(0)
    Seq(1000, 2000, 3000).map(i => buf2((index0 + i) % buf.size)).sum
  }

  override def part2(input: Iterator[String]): Long = {
    val inputs = input.map(_.toInt).toArray.map(_ * 811589153L).zipWithIndex

    val buf: ArrayBuffer[(Long, Int)] = ArrayBuffer(ArraySeq.unsafeWrapArray(inputs): _*)

    (1 to 10).foreach {
      case _ =>
        inputs.foreach {
          case element @ (value, _) =>
            val prevIndex = buf.indexOf(element)
            val newIndex  = math.floorMod(prevIndex + value, inputs.size.toLong - 1).toInt
            buf.remove(prevIndex)
            buf.insert(newIndex, element)
        }
    }

    val buf2 = buf.map(_._1)

    val index0 = buf2.indexOf(0)
    Seq(1000, 2000, 3000).map(i => buf2((index0 + i) % buf.size)).sum
  }
}
