import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day9.part1(Day9.input) shouldEqual 508
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day9.part2(Day9.input) shouldEqual 1564640
  }
}
