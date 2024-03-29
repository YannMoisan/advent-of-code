import org.scalatest.prop.TableDrivenPropertyChecks._

class NonRegressionSpec
    extends AllPuzzlesSpec(
      Table(
        ("day", "part", "result"),
        (1, 1, 1215),
        (1, 2, 1150),
        (2, 1, 1635930),
        (2, 2, 1781819478),
        (3, 1, 2035764),
        (3, 2, 2817661),
        (4, 1, 25410),
        (4, 2, 2730),
        (5, 1, 5608),
        (5, 2, 20299),
        (6, 1, 350149),
        (6, 2, 1590327954513L),
        (7, 1, 356958),
        (7, 2, 105461913),
        (8, 1, 355),
        (8, 2, 983030),
        (9, 1, 508),
        (9, 2, 1564640),
        (10, 1, 387363),
        (10, 2, 4330777059L),
        (11, 1, 1661),
        (11, 2, 334),
        (12, 1, 4413),
        (12, 2, 118803),
        (14, 1, 3342),
        (14, 2, 3776553567525L),
        (16, 1, 957),
        (16, 2, 744953223228L),
        (17, 1, 9870),
        (17, 2, 5523),
        (23, 1, 18051),
        (23, 2, 50245),
        (25, 1, 492)
      )
    )
