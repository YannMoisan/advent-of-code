import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class IntervalSpec extends AnyFlatSpec with Matchers {
  behavior of "merge two intervals"

  it should "merge disjoint intervals" in {
    val a = Interval(2, 4)
    val b = Interval(6, 7)
    Interval.merge(a, b) shouldEqual Seq(a, b)
  }

  it should "merge disjoint intervals (switch)" in {
    val a = Interval(2, 4)
    val b = Interval(6, 7)
    Interval.merge(b, a) shouldEqual Seq(a, b)
  }

  it should "merge included intervals" in {
    val a = Interval(2, 10)
    val b = Interval(4, 6)
    Interval.merge(a, b) shouldEqual Seq(a)
  }

  it should "merge included intervals (switch)" in {
    val a = Interval(2, 10)
    val b = Interval(4, 6)
    Interval.merge(b, a) shouldEqual Seq(a)
  }

  it should "merge included intervals same inf" in {
    val a = Interval(2, 4)
    val b = Interval(2, 6)
    Interval.merge(a, b) shouldEqual Seq(b)
  }

  it should "merge included intervals same inf (switch)" in {
    val a = Interval(2, 4)
    val b = Interval(2, 6)
    Interval.merge(b, a) shouldEqual Seq(b)
  }

  it should "merge included intervals same sup" in {
    val a = Interval(2, 10)
    val b = Interval(5, 10)
    Interval.merge(a, b) shouldEqual Seq(a)
  }

  it should "merge included intervals same sup (switch)" in {
    val a = Interval(2, 10)
    val b = Interval(5, 10)
    Interval.merge(b, a) shouldEqual Seq(a)
  }

  it should "merge overlapped intervals" in {
    val a = Interval(2, 10)
    val b = Interval(5, 15)
    Interval.merge(a, b) shouldEqual Seq(Interval(2, 15))
  }

  it should "merge overlapped intervals (switch)" in {
    val a = Interval(2, 10)
    val b = Interval(5, 15)
    Interval.merge(b, a) shouldEqual Seq(Interval(2, 15))
  }

  behavior of "merge multiple intervals"

  it should "merge disjoint intervals" in {
    val a = Interval(2, 4)
    val b = Interval(2, 10)
    val c = Interval(9, 12)
    Interval(-551671, 3409989)
    Interval(3547229, 3767655)
    Interval(3409991, 4071379)
    Interval(3409991, 4091997)

    Interval.merge(List(a, b, c)) shouldEqual Seq(Interval(2, 12))
    Interval.merge(List(a, c, b)) shouldEqual Seq(Interval(2, 12))
    Interval.merge(List(b, a, c)) shouldEqual Seq(Interval(2, 12))
    Interval.merge(List(b, c, a)) shouldEqual Seq(Interval(2, 12))
    Interval.merge(List(c, a, b)) shouldEqual Seq(Interval(2, 12))
    Interval.merge(List(c, b, a)) shouldEqual Seq(Interval(2, 12))
  }

}
