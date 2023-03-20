import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 599),
        (1, 2, 81204),
        (2, 1, 7936),
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
        (8, 2, 32850)
      )
    )
