import com.yannmoisan.aoc.Day19
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day19Spec extends FlatSpec with Matchers {
  "Day19" should "answer part1" in {
    part1(Day19) should === (1834903)
  }

  it should "answer part2" in {
    part2(Day19) should === (1420280)
  }
}