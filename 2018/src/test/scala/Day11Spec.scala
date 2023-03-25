import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {

  it should "work on examples" in {
    Day11.power(3, 5, 8) shouldEqual 4
    Day11.power(122, 79, 57) shouldEqual -5
    Day11.power(217, 196, 39) shouldEqual 0
    Day11.power(101, 153, 71) shouldEqual 4
  }
}
