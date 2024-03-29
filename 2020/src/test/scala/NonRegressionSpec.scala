import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 870331),
        (1, 2, 283025088),
        (2, 1, 519),
        (2, 2, 708),
        (3, 1, 272L),
        (3, 2, 3898725600L),
        (4, 1, 182),
        (4, 2, 109),
        (5, 1, 996),
        (5, 2, 671),
        (6, 1, 6748),
        (6, 2, 3445),
        (7, 1, 248),
        (7, 2, 57281),
        (8, 1, 1446),
        (8, 2, 1403),
        (11, 1, 2412),
        (11, 2, 2176),
        (12, 1, 1565),
        (12, 2, 78883),
        (14, 1, 7477696999511L),
        (14, 2, 3687727854171L),
        (15, 1, 959),
        (15, 2, 116590),
        (16, 1, 28884),
        (16, 2, 1001849322119L),
        (17, 1, 247),
        (17, 2, 1392),
        (22, 1, 32448),
        (22, 2, 32949),
        (23, 1, 54896723L),
        (23, 2, 146304752384L),
        (24, 1, 346),
        (24, 2, 3802)
      )
    )
