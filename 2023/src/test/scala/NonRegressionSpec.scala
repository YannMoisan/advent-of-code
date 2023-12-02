import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 54159),
        (1, 2, 53866),
        (2, 1, 2317),
        (2, 2, 74804)
      )
    )
