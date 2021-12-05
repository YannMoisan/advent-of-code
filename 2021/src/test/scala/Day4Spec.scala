import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day4.part1(Day4.input) shouldEqual 25410
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day4.part2(Day4.input) shouldEqual 2730
  }
}
