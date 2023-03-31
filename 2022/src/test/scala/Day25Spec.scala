import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day25Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

// Decimal          SNAFU
  //        1              1
  //        2              2
  //        3             1=
  //        4             1-
  //        5             10
  //        6             11
  //        7             12
  //        8             2=
  //        9             2-
  //       10             20
  //       15            1=0
  //       20            1-0
  //     2022         1=11-2
  //    12345        1-0---0
  //314159265  1121-1110-1=0

  it should "convert from snafu numbers" in {
    Day25.snafuToInt("1=") shouldEqual 3
    Day25.snafuToInt("1=11-2") shouldEqual 2022
    Day25.snafuToInt("1-0---0") shouldEqual 12345
    Day25.snafuToInt("1121-1110-1=0") shouldEqual 314159265
  }

  it should "convert to snafu numbers" in {
    Day25.intToSnafu(3) shouldEqual "1="
    Day25.intToSnafu(2022) shouldEqual "1=11-2"
    Day25.intToSnafu(12345) shouldEqual "1-0---0"
    Day25.intToSnafu(314159265) shouldEqual "1121-1110-1=0"
  }
}
