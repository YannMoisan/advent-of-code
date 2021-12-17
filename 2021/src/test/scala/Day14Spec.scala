import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day14.part1(Day14.input) shouldEqual 3342
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day14.part2(Day14.input) shouldEqual 3776553567525L
  }
}
