import com.yannmoisan.aoc.Day21
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day21Spec extends FlatSpec with Matchers {
  "Day21" should "answer part1" in {
    part1(Day21) should === ("gfdhebac")
  }

  it should "answer part2" in {
    part2(Day21) should === ("dhaegfbc")
  }
}