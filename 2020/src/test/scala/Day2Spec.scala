import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day2.part1(Day2.input) shouldEqual 519
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day2.part2(Day2.input) shouldEqual 708
  }
}
