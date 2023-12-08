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
        (3, 2, 84584891),
        (4, 1, 18653),
        (4, 2, 5921508),
        (6, 1, 1159152),
        (6, 2, 41513103),
        (7, 1, 250474325),
        (7, 2, 248909434),
        (8, 1, 17287),
        (8, 2, 18625484023687L)
      )
    )
