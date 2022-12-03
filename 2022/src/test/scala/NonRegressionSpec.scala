import org.scalatest.prop.TableDrivenPropertyChecks._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class NonRegressionSpec extends AllPuzzlesSpec(
  Table(
    ("day", "part", "result"),
    (1, 1, 74198),
    (1, 2, 209914),
    (2, 1, 13526),
    (2, 2, 14204)
  )
)
