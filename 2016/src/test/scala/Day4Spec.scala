import com.yannmoisan.aoc.Day4
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day4Spec extends FlatSpec with Matchers {
  "Day4" should "answer part1" in {
    part1(Day4) should === (185371)
  }

  it should "answer part2" in {
    part2(Day4) should === ("984")
  }
}
