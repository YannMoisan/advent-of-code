import com.yannmoisan.aoc.Day24
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day24Spec extends FlatSpec with Matchers {
  "Day24" should "answer part1" in {
    part1(Day24) should === (474)
  }

  it should "answer part2" in {
    part2(Day24) should === (696)
  }
}