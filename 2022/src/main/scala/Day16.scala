import com.yannmoisan.util.graph.BFS

object Day16 extends MultiPuzzle[Int, Int] {
  case class Valve(name: String, rate: Int, children: Seq[String])

  case class State(minute: Int, opened: Set[String], current: String, released: Int)

  sealed trait Action extends Product with Serializable

  case class Open(valve: String) extends Action

  case class Move(valve: String) extends Action

  object Action {
    def possibleActions(valves: Map[String, Valve], s: State2, current: String): Seq[Action] =
      if (s.minute == 30) Seq.empty
      else {
        val open = Option.when(valves(current).rate > 0 && !s.opened.contains(current)) {
          Open(current)
        }

        val moves: Seq[Action] =
          if (s.minute == 29) Seq.empty
          else {
            valves(current).children.map(dst => Move(dst))
          }
        open.fold(moves)(moves.+:(_))
      }
  }

  def next(valves: Map[String, Valve])(s: State): Seq[State] =
    if (s.minute == 30) Seq.empty
    else {
      val open = Option.when(valves(s.current).rate > 0 && !s.opened.contains(s.current)) {
        State(
          s.minute + 1,
          s.opened + s.current,
          s.current,
          s.released + (30 - s.minute) * valves(s.current).rate
        )
      }

      val moves =
        if (s.minute == 29) Seq.empty
        else {
          valves(s.current).children.map { dst =>
            State(
              s.minute + 1,
              s.opened,
              dst,
              s.released
            )
          }
        }
      open.toSeq ++ moves
    }

  case class State2(minute: Int, opened: Set[String], currents: Seq[String], released: Int)

  def next2(valves: Map[String, Valve])(s: State2): Seq[State2] =
    if (s.minute == 30) Seq.empty
    else {

      val actions0: Seq[Action] = Action.possibleActions(valves, s, s.currents(0))
      val actions1: Seq[Action] = Action.possibleActions(valves, s, s.currents(1))

      for {
        a0 <- actions0
        a1 <- actions1
      } yield {
        (a0, a1) match {
          case (Open(v1), Open(v2)) =>
            State2(
              s.minute + 1,
              s.opened ++ Set(v1, v2),
              s.currents,
              s.released + (30 - s.minute) * valves(v1).rate + (30 - s.minute) * valves(v2).rate
            )
          case (Open(v1), Move(v2)) =>
            State2(
              s.minute + 1,
              s.opened + v1,
              Seq(v1, v2),
              s.released + (30 - s.minute) * valves(v1).rate
            )
          case (Move(v1), Open(v2)) =>
            State2(
              s.minute + 1,
              s.opened + v2,
              Seq(v1, v2),
              s.released + (30 - s.minute) * valves(v2).rate
            )
          case (Move(v1), Move(v2)) =>
            State2(
              s.minute + 1,
              s.opened,
              Seq(v1, v2),
              s.released
            )
        }
      }
    }

  override def part1(input: Iterator[String]): Int = {
    val graph = input.map {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $children" =>
        (name, Valve(name, rate.toInt, children.split(",").toIndexedSeq.map(_.trim)))
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $children" =>
        (name, Valve(name, rate.toInt, Seq(children)))
    }.toMap

    val init = State(1, Set.empty, "AA", 0)

    val res = BFS.breadth_first_traverse_no_path_it(init, next(graph))
    res.filter(_.minute >= 29).maxBy(_.released).released
  }

  override def part2(input: Iterator[String]): Int = {
    val graph = input.map {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $children" =>
        (name, Valve(name, rate.toInt, children.split(",").toIndexedSeq.map(_.trim)))
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $children" =>
        (name, Valve(name, rate.toInt, Seq(children)))
    }.toMap

    val init2 = State2(5, Set.empty, Seq("AA", "AA"), 0)

    val res = BFS.breadth_first_traverse_no_path_it(init2, next2(graph))
    res.filter(_.minute >= 29).maxBy(_.released).released
  }
}
