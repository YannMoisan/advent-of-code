import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day12.part1(Day12.input) shouldEqual 1565
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day12.part2(Day12.input) shouldEqual 78883
  }
}
