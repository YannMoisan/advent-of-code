import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day1.part1(Day1.input) shouldEqual 280
  }

  behavior of "Part 2"

  it should "work on example 1" in {
    Day1.part2("(") shouldEqual 1
  }

  it should "work on example 2" in {
    Day1.part2("()())") shouldEqual 5
  }

  it should "work on input" in {
    Day1.part2(Day1.input) shouldEqual 1797
  }

}
