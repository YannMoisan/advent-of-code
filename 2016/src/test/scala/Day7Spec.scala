import com.yannmoisan.aoc.Day7
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day7Spec extends FlatSpec with Matchers {
  "Day7" should "answer part1" in {
    part1(Day7) should === (118)
  }

  it should "answer part2" in {
    part2(Day7) should === (260)
  }
}
