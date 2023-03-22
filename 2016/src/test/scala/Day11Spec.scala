import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  it should "work on aM" in {
    Day11.isValidFloor(Set("aM")) shouldBe true
  }

  it should "work on aG" in {
    Day11.isValidFloor(Set("aG")) shouldBe true
  }

  it should "work on aM aG" in {
    Day11.isValidFloor(Set("aM", "aG")) shouldBe true
  }

  it should "work on aM bM" in {
    Day11.isValidFloor(Set("aM", "bM")) shouldBe true
  }

  it should "work on aM bG" in {
    Day11.isValidFloor(Set("aM", "bG")) shouldBe false
  }

  it should "work on aM bM bG" in {
    Day11.isValidFloor(Set("aM", "bM", "bG")) shouldBe false
  }
}
