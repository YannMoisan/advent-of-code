import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day7.part1(Day7.input) shouldEqual 248
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day7.part2(Day7.input) shouldEqual 57281
  }
}
