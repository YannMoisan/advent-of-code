import Day23.{HallwayToRoom, RoomToHallway, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Spec extends AnyFlatSpec with Matchers {

  behavior of "next"

  it should "compute the next state" in {
    val init = State(
      2,
      List("B", /*"D", "D",*/ "D"),
      List("C", /*"C", "B",*/ "D"),
      List("C", /*"B", "A",*/ "A"),
      List("B", /*"D", "C",*/ "A"),
      Vector.fill(7)(".")
    )
    Day23.next(init, RoomToHallway("A", 1))._1 shouldEqual (
      State(
        2,
        List( /*"D", "D",*/ "D"),
        List("C", /*"C", "B",*/ "D"),
        List("C", /*"B", "A",*/ "A"),
        List("B", /*"D", "C",*/ "A"),
        Vector(".", "B", ".", ".", ".", ".", ".")
      )
    )
  }

  behavior of "cost"

  val s2 = State(
    2,
    List("A"),
    List("B", "B"),
    List.empty,
    List.empty,
    Vector(".", ".", ".", ".", ".", "C", ".")
  )

  it should "work on Room A ToHallway" in {
    Day23.next(s2, RoomToHallway("A", 1))._2 shouldEqual 3
  }

  it should "work on Room B ToHallway" in {
    Day23.next(s2, RoomToHallway("B", 1))._2 shouldEqual 40
  }

  it should "work on Hallway to Room C" in {
    Day23.next(s2, HallwayToRoom(6, "C"))._2 shouldEqual 600
  }
}
