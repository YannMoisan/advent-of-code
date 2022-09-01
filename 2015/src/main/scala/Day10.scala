import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day10 extends SinglePuzzle[String, String] {
  // conclusion 1 : ArrayBuffer > ListBuffer

  // [day=10] (t=351018ms) 492982
  //               1709308ms
  override def part1(input: String): String = {
    val input = "1321131112"

    println(Iterator.iterate(input)(lookAndSay).take(51).toList.last.length)

    println(lookAndSay(input))
    "42"
  }

  override def part2(input: String): String = "???"

  def lookAndSay(s: String): String = {
    if (s.length < 100) println(s)
    println(s"-${s.length}")
    val buf = new mutable.ArrayBuffer[(Char, Int)](s.length)
    (0 until s.length).foreach { i =>
      if (i == 0) {
        buf.addOne((s(0), 1))
      } else {
        val Some((lastCh, lastCount)) = buf.lastOption
        if (lastCh == s(i)) {
          val _ = buf.remove(buf.length - 1)
          buf.addOne((lastCh, lastCount + 1))
        } else
          buf.addOne((s(i), 1))
      }
    }
    val arr = Array.ofDim[Char](buf.length * 2)
    buf.indices.foreach { i =>
      arr(i * 2) = (buf(i)._2 + 48).toChar
      arr(i * 2 + 1) = buf(i)._1
    }
    new String(arr)
  }

//  def lookAndSay(s: String): String = {
//    println(s"-${s.length}")
//    val buf = mutable.ListBuffer[(Char, Int)]()
//    (0 until s.length).foreach { i =>
//      if (i == 0) {
//        buf.addOne((s(0), 1))
//      } else {
//        val Some((lastCh, lastCount)) = buf.lastOption
//        if (lastCh == s(i)) {
//          val _ = buf.remove(buf.length - 1)
//          buf.addOne((lastCh, lastCount + 1))
//        } else
//          buf.addOne((s(i), 1))
//      }
//    }
//    buf.map { case (ch, count) => s"$count$ch" }.mkString
//  }
}
