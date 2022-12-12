import org.scalatest.prop.TableDrivenPropertyChecks._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class NonRegressionSpec extends AllPuzzlesSpec(
  Table(
    ("day", "part", "result"),
    (1, 1, 74198),
    (1, 2, 209914),
    (2, 1, 13526),
    (2, 2, 14204),
    (3, 1, 7742),
    (3, 2, 2276),
    (4, 1, 526),
    (4, 2, 886),
    (5, 1, "FWNSHLDNZ"),
    (5, 2, "RNRGDNFQG"),
    (6, 1, 1566),
    (6, 2, 2265),
    (7, 1, 1477771),
    (7, 2, 3579501),
    (8, 1, 1690),
    (8, 2, 535680),
    (9, 1, 5695),
    (9, 2, 2434),
    (10, 1, 14860),
    (10, 2, """###...##..####.####.#..#.#..#.###..#..#.
              |#..#.#..#....#.#....#..#.#..#.#..#.#.#..
              |#..#.#......#..###..####.#..#.#..#.##...
              |###..#.##..#...#....#..#.#..#.###..#.#..
              |#.#..#..#.#....#....#..#.#..#.#.#..#.#..
              |#..#..###.####.####.#..#..##..#..#.#..#.""".stripMargin),
    (11,1,58794L),
    (11,2,20151213744L),
    (12, 1, 423),
    (12, 2, 416),
  )
)
