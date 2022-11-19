import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor3

class AllPuzzlesSpec(answers: TableFor3[Int, Int, Any]) extends AnyFlatSpec with Matchers {
  forAll(answers) { (d: Int, p: Int, res: Any) =>
    Puzzles.findPuzzles().find(_.day() == s"$d") match {
      case Some(puzzle) =>
        p match {
          case 1 => puzzle.part1(puzzle.input) shouldBe res
          case 2 => puzzle.part2(puzzle.input) shouldBe res
        }
      case None => sys.error(s"Unknown day '$d'")
    }
  }
}
