import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 980),
        (1, 2, 5961),
        (2, 1, 26255179562L),
        (2, 2, 31680313976L),
        (3, 1, 17452),
        (3, 2, 173300819005913L)
      )
    )
