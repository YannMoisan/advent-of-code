import com.yannmoisan.aoc.Day3
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day3Spec extends FlatSpec with Matchers {
  "Day3" should "answer part1" in {
    part1(Day3) should === (993)
  }

  it should "answer part2" in {
    part2(Day3) should === (1849)
  }
}
