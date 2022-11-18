
object Day11 extends MultiPuzzle[String, String] {
  type Floor = Set[String]

  case class State(floors: Seq[Floor], currentFloor: Int)

  val possibleFloors = Map(0 -> Seq(1), 1 -> Seq(0, 2), 2 -> Seq(1, 3), 3 -> Seq(2))

  def moves(s: State): Seq[State] = {
    // all combination of one or two elem on the current floor
    val current = s.floors(s.currentFloor)
    def possibleMoves(i: Int) = {
      if (i == 1) // up
        current.subsets(2) ++ current.subsets(1)
      else // down
        current.filter(_ (1) == 'M').subsets(1)
    }
    for {
      f <- possibleFloors(s.currentFloor)
      m <- possibleMoves(f - s.currentFloor)
      ns <- update(s, f, m)
    } yield ns
  }

  def update(s: State, destFloor: Int, objects: Set[String]): Option[State] = {
    val newCurFloor = s.floors(s.currentFloor).filterNot(objects.contains)
    val newDstFloor = s.floors(destFloor) ++ objects
    val newState = s.copy(floors = s.floors
      .updated(s.currentFloor, newCurFloor)
      .updated(destFloor, newDstFloor),
      currentFloor = destFloor
    )
    if (isValidFloor(newCurFloor) && isValidFloor(newDstFloor)) Some(newState) else None
  }

  def isValidFloor(f: Floor) = {
    //is there a chip alone and a generator alone
    val grouped: Map[Char, Seq[String]] = f.toList.groupBy(_ (0))
    val alone = grouped.filter { case (_, s) => s.size != 2 }
    val alone2 = alone.values.flatten.toList
    !alone2.exists(_ (1) == 'M') || !alone2.exists(_ (1) == 'G')
  }

  def init0 = State(
    Seq(
      Set("HM", "LM"),
      Set("HG"),
      Set("LG"),
      Set()), 0
  )

  def init1 = State(
    Seq(
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM"),
      Set("pM", "PM"),
      Set(),
      Set()), 0
  )

  def init2 = State(
    Seq(
      Set("EM", "DM", "EG", "DG"),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM")), 3
  )

  def final0 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("HM", "LM", "HG", "LG")
    ), 3
  )

  def final1 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM")
    ), 3
  )

  def final2 = State(
    Seq(
      Set(),
      Set(),
      Set(),
      Set("PG", "TG", "TM", "pG", "RG", "RM", "CG", "CM", "pM", "PM", "EM", "DM", "EG", "DG")
    ), 3
  )

  var cache = collection.mutable.Set[State]()
  val queue = collection.mutable.Queue[(State, Int)]()
  var found = false

  def part(init: State, finalS: State) = { lines: Seq[String] =>
    moves(init).foreach { s => queue += s -> 0 }
    while (!queue.isEmpty && !found) {
      val (next, i) = queue.dequeue()
      if (!cache.contains(next)) {
        cache.add(next)
        if (next == finalS) {
          found = true;
          println(s"final = $i")
        } else ()
        moves(next).foreach { s => queue += s -> (i + 1) }
      }
    }
    "42"
  }

  override def part1(lines: Iterator[String]): String = part(init1, final1)(lines.toList)

  override def part2(lines: Iterator[String]): String = part(init2, final2)(lines.toList)

  //47
  //71
}
