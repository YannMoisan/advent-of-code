import scala.collection.mutable

object Day21 extends MultiPuzzle[Int, Long] {
  case class State(die: Int, player1: Int, player2: Int)

  override def part1(input: Iterator[String]): Int = {
    // pos are from 0 to 9 so we can use a modulo
    var p1    = 0
    var p2    = 0
    var p1pos = 7
    var p2pos = 5
    val dice  = Iterator.from(0).map(_ % 100).map(_ + 1)
    var isP1  = true
    var turn  = 0
    while (p1 < 1000 && p2 < 1000) {
      if (isP1) {
        val roll = dice.next() + dice.next() + dice.next()
        //rolls += roll

        p1pos = (p1pos + roll) % 10
        p1 += (p1pos + 1)
      } else {
        val roll = dice.next() + dice.next() + dice.next()
        //rolls += roll
        p2pos = (p2pos + roll) % 10
        p2 += (p2pos + 1)
      }
      //if (turn < 10) println(s"p1=$p1,p2=$p2,turn=$turn")
      isP1 = !isP1
      turn += 3
    }
    println(s"p1=$p1,p2=$p2,turn=$turn")
    math.min(p1, p2) * turn
  }

  override def part2(input: Iterator[String]): Long = {
    val (w1, w2) = play(7, 5, 0, 0)
    math.max(w1, w2)
  }

  val DP = mutable.Map[(Int, Int, Int, Int), (Long, Long)]()

  def play(p1pos: Int, p2pos: Int, p1score: Int, p2score: Int): (Long, Long) =
    if (p1score >= 21) (1L, 0L)
    else if (p2score >= 21) (0L, 1L)
    else
      DP.getOrElseUpdate(
        (p1pos, p2pos, p1score, p2score), {
          var w1 = 0L
          var w2 = 0L
          for {
            d1 <- 1 to 3
            d2 <- 1 to 3
            d3 <- 1 to 3
          } {
            val roll     = d1 + d2 + d3
            val np1pos   = (p1pos + roll) % 10
            val np1score = p1score + np1pos + 1
            val (x1, y1) = play(p2pos, np1pos, p2score, np1score)
            w1 += y1
            w2 += x1
          }
          (w1, w2)
        }
      )
}
