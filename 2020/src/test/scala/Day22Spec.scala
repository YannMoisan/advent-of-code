import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day22Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day22.part1(Day22.input) shouldEqual 32448
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day22.part2(Day22.input) shouldEqual 32949
  }
}
