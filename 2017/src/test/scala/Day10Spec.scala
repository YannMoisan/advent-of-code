import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {

  it should "work on given examples" in {
    KnotHash.hash("") shouldEqual "a2582a3a0e66e6e86e3812dcb672a272"
    KnotHash.hash("1,2,3") shouldEqual "3efbe78a8d82f29979031a4aa0b16a9d"
    KnotHash.hash("1,2,4") shouldEqual "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}
