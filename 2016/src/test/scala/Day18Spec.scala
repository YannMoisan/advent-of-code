import com.yannmoisan.aoc.Day18
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day18Spec extends FlatSpec with Matchers {
  "Day18" should "answer part1" in {
    part1(Day18) should === (1963)
  }

  it should "answer part2" in {
    part2(Day18) should === (20009568)
  }
}