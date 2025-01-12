// overflow again
// 23996653
// 6386640365805
object Day9 extends SinglePuzzle[Long, Long] {
  override def part1(input: String): Long = {
    // first traversal to get the length
    val length      = input.map(_.toString.toInt).sum
    val buffer      = Array.ofDim[Int](length)
    var isFree      = false
    var bufferIndex = 0
    var blockIndex  = 0
    (0 until input.length).foreach { i =>
      val blockLength = input(i).toString.toInt
      (0 until blockLength).foreach(j => buffer(bufferIndex + j) = (if (isFree) -1 else blockIndex))
      if (!isFree) blockIndex += 1
      bufferIndex += blockLength
      isFree = !isFree
    }
    compact(buffer)
    checksum(buffer)
    // 00...111...2...333.44.5555.6666.777.888899
    // 00...111...2...333.44.5555.6666.777.888899

    // 0099811188827773336446555566..............
    // 0099811188827773336446555566..............

  }

  // very naive because travaersal from start and end each time
  def compact(buffer: Array[Int]): Unit = {
    var fst = buffer.indexOf(-1)
    var lst = buffer.lastIndexWhere(_ != -1)
    while (fst < lst) {
      buffer(fst) = buffer(lst)
      buffer(lst) = -1
      fst = buffer.indexOf(-1)
      lst = buffer.lastIndexWhere(_ != -1)
    }
  }

  // very naive because traversal from start and end each time
  def compact2(buffer0: Array[Block]): Array[Block] = {
    var buffer = buffer0.clone()
    var i      = buffer.length - 1
    while (i >= 0) {
      //println(buffer.mkString(","))
      //(buffer.length - 1 to 0 by -1).foreach { i =>
      // If it is a File, find a free space
      if (buffer(i).isInstanceOf[File]) {
        val candidate = (0 until i).indexWhere(j =>
          buffer(j).isInstanceOf[Free] && buffer(j).asInstanceOf[Free].length >= buffer(i)
            .asInstanceOf[File].length
        )
        if (candidate != -1) {
          // swap
          if (buffer(candidate).asInstanceOf[Free].length == buffer(i).asInstanceOf[File].length) {
            val tmp = buffer(candidate)
            buffer(candidate) = buffer(i)
            buffer(i) = tmp
            i -= 1
          } else {
            val diff =
              buffer(candidate).asInstanceOf[Free].length - buffer(i).asInstanceOf[File].length
            //val tmp = buffer(candidate)
            buffer(candidate) = buffer(i)
            // insert into the array

            buffer = buffer.take(candidate + 1) ++ Array(Free(diff)) ++ buffer.drop(candidate + 1)

            //buffer(candidate+1) = Free(diff)
            buffer(i + 1) = Free(buffer(candidate).asInstanceOf[File].length)
          }
        } else {
          i -= 1
        }
      } else {
        i -= 1
      }
    }
    buffer
  }
  // à remplacer avec un nouveau free space

  def checksum2(buffer: Array[Block]): Long = {
    var bufferIndex = 0
    var sum         = 0L
    (0 until buffer.length).foreach { i =>
      buffer(i) match {
        case File(length, id) =>
          (0 until length).foreach(j => sum += (bufferIndex + j) * id)
          bufferIndex += length
        case Free(length) =>
          bufferIndex += length
      }
    }
    sum
  }

  def checksum(buffer: Array[Int]): Long =
    (0 until buffer.length).map(i => i * (if (buffer(i) == -1) 0 else buffer(i).toLong)).sum

  override def part2(input: String): Long = {
    // first traversal to get the length
    var isFree     = false
    var blockIndex = 0
    val buffer     = Array.ofDim[Block](input.length)
    (0 until input.length).foreach { i =>
      val blockLength = input(i).toString.toInt
      buffer(i) = (if (isFree) Free(blockLength) else File(blockLength, blockIndex))
      //(0 until blockLength).foreach(j => buffer(bufferIndex + j) = (if (isFree) -1 else blockIndex))
      if (!isFree) blockIndex += 1
      //bufferIndex += blockLength
      isFree = !isFree
    }
    val buffer2 = compact2(buffer)
    println(buffer2.mkString(","))
    checksum2(buffer2)
    // File(2,0),File(2,9),File(3,1),File(3,7),File(1,2),File(2,4),File(3,3),Free(1),Free(3),Free(1),File(4,5),Free(1),File(4,6),Free(1),Free(3),Free(1),File(4,8),Free(0),Free(3)
    // 00992111777.44.333....5555.6666.....8888..
    // File(2,0),File(2,9),File(1,2),File(3,1),File(3,7),Free(1),File(2,4),Free(1),File(3,3),Free(1),Free(3),Free(1),File(4,5),Free(1),File(4,6),Free(1),Free(3),Free(1),File(4,8),Free(0),Free(3)

    // File(2,0),File(2,9),File(1,2),File(3,1),File(3,7),Free(1),File(2,4),Free(1),File(3,3),Free(1),Free(3),Free(1),File(4,5),Free(1),File(4,6),Free(1),Free(3),Free(1),File(4,8),Free(0),Free(3)
  }

  sealed trait Block
  case class Free(length: Int)          extends Block
  case class File(length: Int, id: Int) extends Block

}
