import com.yannmoisan.aoc.Day13
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day13Spec extends FlatSpec with Matchers {
  "Day13" should "answer part1" in {
    part1(Day13) should === (92)
  }

  it should "answer part2" in {
    part2(Day13) should === (124)
  }
}