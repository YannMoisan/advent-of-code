import Day11.Foo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  it should "work on PM" in {
    Day11.isValidFloor(Set("PM").map(Foo.apply)) shouldBe true
  }

  it should "work on PG" in {
    Day11.isValidFloor(Set("PG").map(Foo.apply)) shouldBe true
  }

  it should "work on PM PG" in {
    Day11.isValidFloor(Set("PM", "PG").map(Foo.apply)) shouldBe true
  }

  it should "work on PM TM" in {
    Day11.isValidFloor(Set("PM", "TM").map(Foo.apply)) shouldBe true
  }

  it should "work on PM TG" in {
    Day11.isValidFloor(Set("PM", "TG").map(Foo.apply)) shouldBe false
  }

  it should "work on PM TM TG" in {
    Day11.isValidFloor(Set("PM", "TM", "TG").map(Foo.apply)) shouldBe false
  }
}
