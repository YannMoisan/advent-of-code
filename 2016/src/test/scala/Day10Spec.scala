import com.yannmoisan.aoc.Day10
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day10Spec extends FlatSpec with Matchers {
  "Day10" should "answer part1" in {
    part1(Day10) should === ("113")
  }

  it should "answer part2" in {
    part2(Day10) should === (12803)
  }
}