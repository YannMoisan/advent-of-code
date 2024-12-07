import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 1320851),
        (1, 2, 26859182),
        (2, 1, 483),
        (2, 2, 528),
        (3, 1, 166905464),
        (3, 2, 72948684),
        (4, 1, 2633),
        (4, 2, 1936),
        (5, 1, 5374),
        (5, 2, 4260),
        (6, 1, 5101),
        (6, 2, 1951)
      )
    )
