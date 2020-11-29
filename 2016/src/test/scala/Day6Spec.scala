import com.yannmoisan.aoc.Day6
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day6Spec extends FlatSpec with Matchers {
  "Day6" should "answer part1" in {
    part1(Day6) should === ("cyxeoccr")
  }

  it should "answer part2" in {
    part2(Day6) should === ("batwpask")
  }
}
