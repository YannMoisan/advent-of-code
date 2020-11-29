import com.yannmoisan.aoc.Day9
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day9Spec extends FlatSpec with Matchers {
  "Day9" should "answer part1" in {
    part1(Day9) should === (102239)
  }

  it should "answer part2" in {
    part2(Day9) should === (10780403063L)
  }
}