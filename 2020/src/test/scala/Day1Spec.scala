import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  "Day1" should "part2" in {
    Day1.part2("(") shouldEqual 1
  }
  "Day1" should "part2b" in {
    Day1.part2("()())") shouldEqual 5
  }
}
