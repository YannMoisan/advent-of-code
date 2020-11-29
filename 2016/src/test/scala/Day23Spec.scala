import com.yannmoisan.aoc.Day23
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day23Spec extends FlatSpec with Matchers {
  "Day23" should "answer part1" in {
    part1(Day23) should === (10584)
  }

  ignore should "answer part2" in {
    part2(Day23) should === (479007144)
  }
}