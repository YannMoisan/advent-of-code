import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 1320851),
        (1, 2, 26859182),
        (2, 1, 483),
        (2, 2, 528)
      )
    )
