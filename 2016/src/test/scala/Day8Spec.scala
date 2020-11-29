import com.yannmoisan.aoc.Day8
import com.yannmoisan.aoc.Puzzles._
import org.scalatest._

class Day8Spec extends FlatSpec with Matchers {
  "Day8" should "answer part1" in {
    part1(Day8) should === (128)
  }

  it should "answer part2" in {
    part2(Day8) should === ("EOARGPHYAO")
  }
}