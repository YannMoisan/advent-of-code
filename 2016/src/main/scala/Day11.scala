import com.yannmoisan.util.graph.BFS

object Day11 extends MultiPuzzle[Int, Int] {
  type Floor = Set[String]

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

  def update(s: State, destFloor: Int, objects: Set[String]): Option[State] = {
    val newCurFloor = s.floors(s.currentFloor).filterNot(objects.contains)
    val newDstFloor = s.floors(destFloor) ++ objects
    val newState = s.copy(
      floors = s.floors
        .updated(s.currentFloor, newCurFloor)
        .updated(destFloor, newDstFloor),
      currentFloor = destFloor
    )
    if (isValidFloor(newCurFloor) && isValidFloor(newDstFloor)) Some(newState) else None
  }

  def isValidFloor(f: Floor) = {
    //is there a chip alone and a generator alone
    val grouped: Map[Char, Seq[String]] = f.toList.groupBy(_(0))
    val alone                           = grouped.filter { case (_, s) => s.size != 2 }
    val alone2                          = alone.values.flatten.toList
    !alone2.exists(_(1) == 'M') || !f.exists(_(1) == 'G')
  }

  def init0 = State(
    Seq(Set("HM", "LM"), Set("HG"), Set("LG"), Set()),
    0
  )

  def init1 = State(
    Seq(Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM"), Set("pM", "PM"), Set(), Set()),
    0
  )

  def init2 = State(
    Seq(
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "EM", "DM", "EG", "DG"),
      Set("pM", "PM"),
      Set(),
      Set()
    ),
    0
  )

  def final0 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("HM", "LM", "HG", "LG")
    ),
    3
  )

  def final1 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM")
    ),
    3
  )

  def final2 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM", "EM", "DM", "EG", "DG")
    ),
    3
  )

  def part(init: State, finalS: State) = { _: Seq[String] =>
    BFS.breadth_first_traverse(init, moves).find(_._1 == finalS).get._2.size - 1
  }

  override def part1(lines: Iterator[String]): Int = part(init1, final1)(lines.toList)

  override def part2(lines: Iterator[String]): Int = part(init2, final2)(lines.toList)

}
