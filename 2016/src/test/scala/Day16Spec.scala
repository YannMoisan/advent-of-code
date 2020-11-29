import com.yannmoisan.aoc.Day16
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day16Spec extends FlatSpec with Matchers {
  "Day16" should "answer part1" in {
    part1(Day16) should === ("01110011101111011")
  }

  it should "answer part2" in {
    part2(Day16) should === ("11001111011000111")
  }
}