import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day10.part1(Day10.input) shouldEqual 387363
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day10.part2(Day10.input) shouldEqual 4330777059L
  }
}
