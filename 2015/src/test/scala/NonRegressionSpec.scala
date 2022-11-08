import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

@SuppressWarnings(
  Array(
    "org.wartremover.warts.AsInstanceOf",
    "org.wartremover.warts.NonUnitStatements",
  )
)
class NonRegressionSpec extends AnyFlatSpec with Matchers {
  val m = Table(
    ("day", "part", "result"),
    (1, 1 , 280),
    (1, 2 , 1797),
    (8, 1 , 1333),
    (8, 2 , 2046)
  )

  forAll(m) { (d: Int, p: Int, res: Any) =>
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
