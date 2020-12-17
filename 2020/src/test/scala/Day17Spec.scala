import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day17.part1(Day17.input) shouldEqual 247
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day17.part2(Day17.input) shouldEqual 1392
  }
}
