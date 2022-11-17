import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

@SuppressWarnings(
  Array(
    "org.wartremover.warts.Any",
    "org.wartremover.warts.AsInstanceOf",
    "org.wartremover.warts.NonUnitStatements"
  )
)
class NonRegressionSpec extends AnyFlatSpec with Matchers {
  val m = Table(
    ("day", "part", "result"),
    (1, 1, 599),
    (1, 2, 81204),
    (2, 1,7936 ),
    (2, 2, "lnfqdscwjyteorambzuchrgpx"),
    (3, 1, 106501),
    (3, 2, 632),
    (4, 1, 19874),
    (4, 2, 22687),
    (5, 1, 11546),
    (5, 2, 5124),
//    (6, 1, 3722),
    (6, 2, 44634),
//    (7, 1, "BFLNGIRUSJXEHKQPVTYOCZDWMA"),
//    (7, 2, 880),
    (8, 1, 48496),
    (8, 2, 32850),
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
