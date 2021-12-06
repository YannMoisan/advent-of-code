import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day5.part1(Day5.input) shouldEqual 5608
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day5.part2(Day5.input) shouldEqual 20299
  }
}
