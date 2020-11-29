import com.yannmoisan.aoc.Day17
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day17Spec extends FlatSpec with Matchers {
  "Day17" should "answer part1" in {
    part1(Day17) should === ("RDDRLDRURD")
  }

  it should "answer part2" in {
    part2(Day17) should === (448)
  }
}