import scala.collection.mutable

object Day11 extends MultiPuzzle[Long, Long] {
  case class Monkey(
                     items: mutable.Queue[Long],
                     op: Long => Long,
                     test: Long => Int,
                     var count: Int
                   )
  def init() = Array(
    Monkey(mutable.Queue(93, 54, 69, 66, 71), _ * 3, x => if (x % 7 == 0) 7 else 1,0),
    Monkey(mutable.Queue(89, 51, 80, 66), _ * 17, x => if (x % 19 == 0) 5 else 7,0),
    Monkey(mutable.Queue(90, 92, 63, 91, 96, 63, 64), _ + 1, x => if (x % 13 == 0) 4 else 3,0),
    Monkey(mutable.Queue(65, 77), _ + 2, x => if (x % 3 == 0) 4 else 6,0),
    Monkey(mutable.Queue(76, 68, 94), x => x * x, x => if (x % 2 == 0) 0 else 6,0),
    Monkey(mutable.Queue(86, 65, 66, 97, 73, 83), _ + 8, x => if (x % 11 == 0) 2 else 3,0),
    Monkey(mutable.Queue(78), _ + 6, x => if (x % 17 == 0) 0 else 1,0),
    Monkey(mutable.Queue(89, 57, 59, 61, 87, 55, 55, 88), _ + 7, x => if (x % 5 == 0) 2 else 5,0),
   )

  private def round1(monkeys: Array[Monkey]) : Unit = {
    monkeys.indices.foreach {
      i => while (!monkeys(i).items.isEmpty) {
        monkeys(i).count += 1
        val item = monkeys(i).items.dequeue()
        val itemAfterOp = monkeys(i).op(item)
        val itemAfterDiv = math.floor(itemAfterOp.toDouble / 3).toLong
        monkeys(monkeys(i).test(itemAfterDiv)).items.enqueue(itemAfterDiv)
      }
    }
  }

  private def round2(monkeys: Array[Monkey]): Unit = {
    monkeys.indices.foreach {
      i =>
        while (!monkeys(i).items.isEmpty) {
          monkeys(i).count += 1
          val item = monkeys(i).items.dequeue()
          val itemAfterOp = monkeys(i).op(item)
          val itemAfterDiv = itemAfterOp % (7 * 19 * 13 * 3 * 2 * 11 * 17 * 5)
          monkeys(monkeys(i).test(itemAfterDiv)).items.enqueue(itemAfterDiv)
        }
    }
  }

  override def part1(input: Iterator[String]): Long = {
    val monkeys = init()
    (1 to 20).foreach(_ => round1(monkeys))
    monkeys.map(m => m.count.toLong).sorted.takeRight(2).product
  }

  override def part2(input: Iterator[String]): Long = {
    val monkeys = init()
    (1 to 10000).foreach(_ => round2(monkeys))
    monkeys.map(m => m.count.toLong).sorted.takeRight(2).product
  }
}
