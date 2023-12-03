import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 54159),
        (1, 2, 53866),
        (2, 1, 2317),
        (2, 2, 74804),
        (3, 1, 540025),
        (3, 2, 84584891)
      )
    )
