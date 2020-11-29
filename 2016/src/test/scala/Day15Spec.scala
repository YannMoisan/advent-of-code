import com.yannmoisan.aoc.Day15
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day15Spec extends FlatSpec with Matchers {
  "Day15" should "answer part1" in {
    part1(Day15) should === (317371)
  }

  it should "answer part2" in {
    part2(Day15) should === (2080951)
  }
}