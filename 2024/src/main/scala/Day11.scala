import scala.collection.mutable

object Day11 extends SinglePuzzle[Int, Long] {
  override def part1(input: String): Int = {
    val l   = input.split(" ").map(_.toLong).toList
    val res = (0 until 25).foldLeft(l) { case (acc, _) => next(acc) }
    println(res.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy(_._2))
    res.size
  }

  override def part2(input: String): Long = {
    val numbers    = input.split(" ").map(_.toLong).toList
    var occurences = mutable.Map[Long, Long]()
    val nextCache  = mutable.Map[Long, List[Long]]()

    numbers.foreach(number => occurences.put(number, 1))
    (0 until 75).foreach { _ =>
      val newOccurences = mutable.Map[Long, Long]()
      occurences.foreach {
        case (number, occ) =>
          nextCache.getOrElseUpdate(number, next0(number)).foreach { nextNumber =>
            newOccurences.update(nextNumber, newOccurences.getOrElse(nextNumber, 0L) + occ)
          }
      }
      occurences = newOccurences
    }
    occurences.map(_._2).sum
  }

  // 1066883794

  //    val l   = input.split(" ").map(_.toLong).toList
//    val res = (0 until 25).foldLeft(l) { case (acc, _) => next(acc) }
//    res.size

  def next(l: List[Long]): List[Long] =
    l.flatMap(next0)

  def next0(i: Long): List[Long] =
    if (i == 0) List(1)
    else if (i.toString.length % 2 == 0)
      List(
        i.toString.take(i.toString.length / 2).toLong,
        i.toString.drop(i.toString.length / 2).toLong
      )
    else
      List(i * 2024)
}

// 199986
