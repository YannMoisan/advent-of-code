import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day23.part1(Day23.input) shouldEqual 54896723L
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day23.part2(Day23.input) shouldEqual 146304752384L
  }
}
