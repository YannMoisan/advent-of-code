import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day8Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day8.part1(Day8.input) shouldEqual 1446
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day8.part2(Day8.input) shouldEqual 1483
  }
}
