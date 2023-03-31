import com.yannmoisan.util.graph.BFS

//import scala.collection.mutable

object Day11 extends MultiPuzzle[Int, Int] {
  case class Foo(k: Int, v: Int)
  object Foo {
    def apply(s: String): Foo = {
      val k = s(0) match {
        case 'P' => 0
        case 'T' => 1
        case 'p' => 2
        case 'R' => 3
        case 'C' => 4
        case 'E' => 5
        case 'D' => 6
      }
      val v = s(1) match {
        case 'M' => 1
        case 'G' => 2
      }
      Foo(k, v)
    }
  }
  type Floor = Set[Foo]

  case class State(floors: Seq[Floor], currentFloor: Int)

  val possibleFloors = Map(0 -> Seq(1), 1 -> Seq(0, 2), 2 -> Seq(1, 3), 3 -> Seq(2))

  def moves(s: State): Seq[State] = {
    // all combination of one or two elem on the current floor
    val current = s.floors(s.currentFloor)
    for {
      f  <- possibleFloors(s.currentFloor)
      m  <- current.subsets(2) ++ current.subsets(1)
      ns <- update(s, f, m)
    } yield ns
  }

  def update(s: State, destFloor: Int, objects: Set[Foo]): Option[State] = {
    val newCurFloor = s.floors(s.currentFloor).filterNot(objects.contains)
    val newDstFloor = s.floors(destFloor) ++ objects
    if (isValidFloor(newCurFloor) && isValidFloor(newDstFloor)) {
      val newState = s.copy(
        floors = s.floors
          .updated(s.currentFloor, newCurFloor)
          .updated(destFloor, newDstFloor),
        currentFloor = destFloor
      )
      Some(newState)
    } else None
  }

  def isValidFloor(f: Floor) = {
    var gs = 0
    var ms = 0
    f.foreach { s =>
      if (s.v == 2)
        gs += 1 << s.k
      else
        ms += 1 << s.k
    }
    gs == 0 || (~gs & ms) == 0
  }

  def init0 = State(
    Seq(
      Set("HM", "LM"),
      Set("HG"),
      Set("LG"),
      Set()
    ).map(_.map(Foo.apply)),
    0
  )

  def init1 = State(
    Seq(Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM"), Set("pM", "PM"), Set(), Set())
      .map(_.map(Foo.apply)),
    0
  )

  def init2 = State(
    Seq(
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "EM", "DM", "EG", "DG"),
      Set("pM", "PM"),
      Set(),
      Set()
    ).map(_.map(Foo.apply)),
    0
  )

  def final0 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("HM", "LM", "HG", "LG")
    ).map(_.map(Foo.apply)),
    3
  )

  def final1 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM")
    ).map(_.map(Foo.apply)),
    3
  )

  def final2 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM", "EM", "DM", "EG", "DG")
    ).map(_.map(Foo.apply)),
    3
  )

  def part(init: State, finalS: State) = { _: Seq[String] =>
    BFS.breadth_first_traverse(init, moves).find(_._1 == finalS).get._2.size - 1
  }

  override def part1(lines: Iterator[String]): Int = part(init1, final1)(lines.toList)

  override def part2(lines: Iterator[String]): Int = part(init2, final2)(lines.toList)

}
