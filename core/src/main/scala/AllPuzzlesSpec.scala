import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor3
import org.scalatest.propspec.AnyPropSpec

class AllPuzzlesSpec(answers: TableFor3[Int, Int, Any]) extends AnyPropSpec with Matchers {
  property("all puzzles produce expected results") {
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
}
