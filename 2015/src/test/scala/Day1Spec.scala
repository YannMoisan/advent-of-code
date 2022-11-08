import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  behavior of "Part 2"

  it should "work on example 1" in {
    Day1.part1("(") shouldEqual 1
  }

  it should "work on example 2" in {
    Day1.part2("()())") shouldEqual 5
  }

}
