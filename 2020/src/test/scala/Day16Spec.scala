import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day16.part1(Day16.input) shouldEqual 28884
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day16.part2(Day16.input) shouldEqual 1001849322119L
  }
}
