import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9Spec extends AnyFlatSpec with Matchers {

  it should "work on examples" in {
    Day9.highScore(9, 25) shouldEqual 32

    /*
     *
        10 players; last marble is worth 1618 points: high score is 8317
        13 players; last marble is worth 7999 points: high score is 146373
        17 players; last marble is worth 1104 points: high score is 2764
        21 players; last marble is worth 6111 points: high score is 54718
        30 players; last marble is worth 5807 points: high score is 37305

     * */

    Day9.highScore(10, 1618) shouldEqual 8317
    Day9.highScore(13, 7999) shouldEqual 146373
    Day9.highScore(17, 1104) shouldEqual 2764
    Day9.highScore(21, 6111) shouldEqual 54718
    Day9.highScore(30, 5807) shouldEqual 37305
  }
}
