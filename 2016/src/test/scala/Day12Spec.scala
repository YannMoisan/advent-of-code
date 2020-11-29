import com.yannmoisan.aoc.Day12
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day12Spec extends FlatSpec with Matchers {
  "Day12" should "answer part1" in {
    part1(Day12) should === (318007)
  }

  it should "answer part2" in {
    part2(Day12) should === (9227661)
  }
}