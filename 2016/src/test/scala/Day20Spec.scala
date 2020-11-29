import com.yannmoisan.aoc.Day20
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day20Spec extends FlatSpec with Matchers {
  "Day20" should "answer part1" in {
    part1(Day20) should === (31053880)
  }

  it should "answer part2" in {
    part2(Day20) should === (117)
  }
}