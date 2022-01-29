import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day23 extends MultiPuzzle[Int, Int] {

  override def part1(input: Iterator[String]): Int = {
    val depth = 2
    val init: State = State(
      depth,
      List("B", /*"D", "D",*/ "D"),
      List("C", /*"C", "B",*/ "D"),
      List("C", /*"B", "A",*/ "A"),
      List("B", /*"D", "C",*/ "A"),
      Vector.fill(7)(".")
    )
    val endState = State(
      depth,
      List.fill(depth)("A"),
      List.fill(depth)("B"),
      List.fill(depth)("C"),
      List.fill(depth)("D"),
      Vector.fill(7)(".")
    )

    bfs(init, endState)
  }

  override def part2(input: Iterator[String]): Int = {
    val depth = 4
    val init: State = State(
      depth,
      List("B", "D", "D", "D"),
      List("C", "C", "B", "D"),
      List("C", "B", "A", "A"),
      List("B", "A", "C", "A"),
      Vector.fill(7)(".")
    )
    val endState = State(
      depth,
      List.fill(depth)("A"),
      List.fill(depth)("B"),
      List.fill(depth)("C"),
      List.fill(depth)("D"),
      Vector.fill(7)(".")
    )
    bfs(init, endState)

  }

  trait Move
  case class RoomToHallway(from: String, to: Int) extends Move
  case class HallwayToRoom(from: Int, to: String) extends Move

  case class State(
      depth: Int,
      arooms: List[String],
      brooms: List[String],
      crooms: List[String],
      drooms: List[String],
      hallway: Vector[String]
  ) {
    def accessible(from: Int): Seq[Int] = accessibleLeft(from) ++ accessibleRight(from + 1)
    private def accessibleLeft(from: Int): Seq[Int] =
      (from to 0 by -1).takeWhile(i => hallway(i) == ".")
    private def accessibleRight(from: Int): Seq[Int] =
      (from to 6 by 1).takeWhile(i => hallway(i) == ".")
    def roomAccessible(from: Int, to: String) =
      ((from, to) match {
        case (0, "A") => Seq(1)
        case (0, "B") => Seq(1, 2)
        case (0, "C") => Seq(1, 2, 3)
        case (0, "D") => Seq(1, 2, 3, 4)
        case (1, "A") => Seq.empty
        case (1, "B") => Seq(2)
        case (1, "C") => Seq(2, 3)
        case (1, "D") => Seq(2, 3, 4)
        case (2, "A") => Seq.empty
        case (2, "B") => Seq.empty
        case (2, "C") => Seq(3)
        case (2, "D") => Seq(3, 4)
        case (3, "A") => Seq(2)
        case (3, "B") => Seq.empty
        case (3, "C") => Seq.empty
        case (3, "D") => Seq(4)
        case (4, "A") => Seq(2, 3)
        case (4, "B") => Seq(3)
        case (4, "C") => Seq.empty
        case (4, "D") => Seq.empty
        case (5, "A") => Seq(2, 3, 4)
        case (5, "B") => Seq(3, 4)
        case (5, "C") => Seq(4)
        case (5, "D") => Seq.empty
        case (6, "A") => Seq(2, 3, 4, 5)
        case (6, "B") => Seq(3, 4, 5)
        case (6, "C") => Seq(4, 5)
        case (6, "D") => Seq(5)
      }).forall(hallway(_) == ".")

    def canMoveToRoom(pos: Int, dst: String): Boolean =
      roomAccessible(pos, dst) && hallway(pos) == dst && (dst match {
        case "A" => arooms.forall(_ == "A")
        case "B" => brooms.forall(_ == "B")
        case "C" => crooms.forall(_ == "C")
        case "D" => drooms.forall(_ == "D")
      })
  }

  val costs = Map(
    ("A", 0) -> 3,
    ("A", 1) -> 2,
    ("A", 2) -> 2,
    ("A", 3) -> 4,
    ("A", 4) -> 6,
    ("A", 5) -> 8,
    ("A", 6) -> 9,
    ("D", 6) -> 3,
    ("D", 5) -> 2,
    ("D", 4) -> 2,
    ("D", 3) -> 4,
    ("D", 2) -> 6,
    ("D", 1) -> 8,
    ("D", 0) -> 9,
    ("B", 0) -> 5,
    ("B", 1) -> 4,
    ("B", 2) -> 2,
    ("B", 3) -> 2,
    ("B", 4) -> 4,
    ("B", 5) -> 6,
    ("B", 6) -> 7,
    ("C", 6) -> 5,
    ("C", 5) -> 4,
    ("C", 4) -> 2,
    ("C", 3) -> 2,
    ("C", 2) -> 4,
    ("C", 1) -> 6,
    ("C", 0) -> 7
  )

  def cost(m: Move): Int =
    m match {
      case HallwayToRoom(from, to) => costs((to, from))
      case RoomToHallway(from, to) => costs((from, to))
    }

  // There are 7 possible positions in the hallway
  // a -> (Seq(1,0), Seq(2,3,4,5,6)
  //  #############
  //  #01.2.3.4.56#
  //  ###A#B#C#D###
  //    #A#B#C#D#
  //    #########

  // 0 A si vide      3 + 2 - 0 = 5
  // 0 A si size =1   3 + 2 - 1 = 4

  // A 0 si size = 1  3 + 2 - 1 = 4
  // A 0 si size = 2  3 + 2 - 2 = 3

  def possibleMoves(s: State): Seq[Move] = {
    // From room to hallway
    val a = s.arooms.headOption.toList.flatMap(_ => s.accessible(1).map(RoomToHallway("A", _))) ++
      s.brooms.headOption.toList.flatMap(_ => s.accessible(2).map(RoomToHallway("B", _))) ++
      s.crooms.headOption.toList.flatMap(_ => s.accessible(3).map(RoomToHallway("C", _))) ++
      s.drooms.headOption.toList.flatMap(_ => s.accessible(4).map(RoomToHallway("D", _)))

    // TODO check if the path is free
    a ++ (0 to 6).flatMap { i =>
      Seq("A", "B", "C", "D").flatMap(str =>
        if (s.canMoveToRoom(i, str))
          Seq(HallwayToRoom(i, str))
        else
          Seq.empty
      )
    }
  }

  def costAmphipod(s: String) = s match {
    case "A" => 1
    case "B" => 10
    case "C" => 100
    case "D" => 1000
  }

  def next(s: State, m: Move): (State, Int) =
    m match {
      case HallwayToRoom(from, "A") =>
        (
          s.copy(arooms = "A" :: s.arooms, hallway = s.hallway.updated(from, ".")),
          cost(m) + (s.depth - s.arooms.size - 1)
        )
      case HallwayToRoom(from, "B") =>
        (
          s.copy(brooms = "B" :: s.brooms, hallway = s.hallway.updated(from, ".")),
          10 * (cost(m) + (s.depth - s.brooms.size - 1))
        )
      case HallwayToRoom(from, "C") =>
        (
          s.copy(crooms = "C" :: s.crooms, hallway = s.hallway.updated(from, ".")),
          100 * (cost(m) + (s.depth - s.crooms.size - 1))
        )
      case HallwayToRoom(from, "D") =>
        (
          s.copy(drooms = "D" :: s.drooms, hallway = s.hallway.updated(from, ".")),
          1000 * (cost(m) + (s.depth - s.drooms.size - 1))
        )
      case RoomToHallway("A", to) =>
        (
          s.copy(arooms = s.arooms.tail, hallway = s.hallway.updated(to, s.arooms.head)),
          costAmphipod(s.arooms.head) * (cost(m) + (s.depth - s.arooms.size))
        )
      case RoomToHallway("B", to) =>
        (
          s.copy(brooms = s.brooms.tail, hallway = s.hallway.updated(to, s.brooms.head)),
          costAmphipod(s.brooms.head) * (cost(m) + (s.depth - s.brooms.size))
        )
      case RoomToHallway("C", to) =>
        (
          s.copy(crooms = s.crooms.tail, hallway = s.hallway.updated(to, s.crooms.head)),
          costAmphipod(s.crooms.head) * (cost(m) + (s.depth - s.crooms.size))
        )
      case RoomToHallway("D", to) =>
        (
          s.copy(drooms = s.drooms.tail, hallway = s.hallway.updated(to, s.drooms.head)),
          costAmphipod(s.drooms.head) * (cost(m) + (s.depth - s.drooms.size))
        )

    }

  def bfs(init: State, endState: State): Int = {
    val q       = mutable.Queue[(State, Int, List[Move])]()
    val visited = mutable.Map[State, Int]()
//    val depth   = 2
//    val endState = State(
//      depth,
//      List.fill(depth)("A"),
//      List.fill(depth)("B"),
//      List.fill(depth)("C"),
//      List.fill(depth)("D"),
//      Vector.fill(7)(".")
//    )

//#############
//#...........#
//###B#C#C#B###
//  #D#D#A#A#
//  #########

    //#############
    //#...........#
    //###B#C#B#D###
    //  #D#C#B#A#
    //  #D#B#A#C#
    //  #A#D#C#A#
    //  #########
//    val init: State = State(
//      depth,
//      List("B", /*"D", "D",*/ "D"),
//      List("C", /*"C", "B",*/ "D"),
//      List("C", /*"B", "A",*/ "A"),
//      List("B", /*"D", "C",*/ "A"),
//      Vector.fill(7)(".")
//    )
    val _     = q.enqueue((init, 0, List.empty))
    var count = 0
    var min   = Integer.MAX_VALUE
    while (!q.isEmpty) {
      val (state, cost, moves) = q.dequeue()

      if (!visited.contains(state) || visited(state) > cost) { //.existscontains(state) || visited.) {
        count += 1

//        if (count % 1000 == 0) {
//          println(count)
//        }
        val pmoves = possibleMoves(state)
        val neighbors: Seq[(State, Int, List[Move])] = pmoves
          .map { move =>
            val (nextState, moveCost) = next(state, move)
            (nextState, moveCost + cost, move :: moves)
          }

        if (neighbors.map(_._1).contains(endState)) {
          println(s"end state reached: cost=${neighbors
            .find(_._1 == endState).map(_._2)}. ")
          val candidate = neighbors.find(_._1 == endState).map(_._2).getOrElse(0)
          if (candidate < min) min = candidate
        }
        val _ = q.enqueueAll(neighbors.filter(_._1 != endState))
        visited(state) = cost
      }
    }
    min
  }

//  val s1 =
//    State(List("A"), List("A"), List("A"), List("A"), Vector(".", ".", ".", "A", ".", ".", "."))
//  println(possibleMoves(s1).length)
//  println(possibleMoves(s1))
//
//  val s2 =
//    State(List("A"), List("A", "B"), List(), List("A"), Vector("A", ".", ".", "A", "C", ".", "."))
//  println(possibleMoves(s2))

//  bfs()
}
