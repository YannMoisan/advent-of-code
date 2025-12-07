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
        (3, 2, 173300819005913L),
        (4, 1, 1367),
        (4, 2, 9144),
        (5, 1, 862),
        (5, 2, 357907198933892L),
        (6, 1, 5060053676136L),
        (6, 2, 9695042567249L)
      )
    )
