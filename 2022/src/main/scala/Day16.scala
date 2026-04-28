import com.yannmoisan.util.graph.BFS

import scala.collection.immutable.ArraySeq

object Day16 extends MultiPuzzle[Int, Int] {
  case class Valve(name: String, rate: Int, children: Seq[String])

  case class IntValve(
      id: Int,
      name: String,
      rate: Int,
      children: Array[Int],
      recChildrenWithFlow: Array[Int]
  )

  case class State(minute: Int, opened: Long, current: Int, released: Int)

  object Action {
    def possibleActions(
        valves: Array[IntValve],
        dist: Array[Array[Int]],
        maxTime: Int,
        state: State
    ): Seq[Int] =
      ArraySeq
        .unsafeWrapArray(valves(state.current).recChildrenWithFlow)
        .filter { v =>
          state.minute + dist(state.current)(v) < maxTime && (state.opened & 1L << v) == 0L
        }
  }

  def next(valves: Array[IntValve], dist: Array[Array[Int]], maxTime: Int)(s: State): Seq[State] =
    Action
      .possibleActions(valves, dist, maxTime, s)
      .map { v =>
        State(
          s.minute + dist(s.current)(v) + 1,
          s.opened | 1L << v,
          v,
          s.released + (maxTime - dist(s.current)(v) - s.minute) * valves(v).rate
        )
      }

  def transform(valves: Map[String, Valve]): Array[IntValve] = {
    val nameToIndex = valves.keys.zipWithIndex.toMap
    val arr         = Array.ofDim[IntValve](valves.size)
    valves.foreach { case (name, valve) =>
      arr(nameToIndex(name)) = IntValve(
        nameToIndex(name),
        name,
        valve.rate,
        valve.children.map(nameToIndex).toArray,
        valves.values.collect {
          case v if v.rate > 0 && v.name != name => nameToIndex(v.name)
        }.toArray
      )
    }
    arr
  }

  override def part1(input: Iterator[String]): Int = {
    val graph = input.map {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $children" =>
        (name, Valve(name, rate.toInt, children.split(",").toIndexedSeq.map(_.trim)))
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $children" =>
        (name, Valve(name, rate.toInt, Seq(children)))
    }.toMap

    val valves = transform(graph)

    val dist = adjMatrix(valves)
    floydWarshall(dist)

    val init = State(1, 0L, valves.indexWhere(_.name == "AA"), 0)

    BFS
      .breadth_first_traverse_no_path_no_visited_it(init, next(valves, dist, 30))
      .maxBy(_.released)
      .released
  }

  override def part2(input: Iterator[String]): Int = {
    val graph = input.map {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $children" =>
        (name, Valve(name, rate.toInt, children.split(",").toIndexedSeq.map(_.trim)))
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $children" =>
        (name, Valve(name, rate.toInt, Seq(children)))
    }.toMap

    val valves = transform(graph)

    val dist = adjMatrix(valves)
    floydWarshall(dist)

    val init = State(1, 0L, valves.indexWhere(_.name == "AA"), 0)

    val endStates = BFS
      .breadth_first_traverse_no_path_no_visited_it(init, next(valves, dist, 26))
      .filter(s => Action.possibleActions(valves, dist, 26, s).isEmpty)

    endStates.map { end =>
      val init2 = State(1, end.opened, valves.indexWhere(_.name == "AA"), end.released)
      BFS
        .breadth_first_traverse_no_path_no_visited_it(init2, next(valves, dist, 26))
        .maxBy(_.released)
        .released
    }.max

  }

  def adjMatrix(arr: Array[IntValve]): Array[Array[Int]] =
    Array.tabulate(arr.length, arr.length) { case (i, j) =>
      if (arr(i).children.contains(j)) 1 else 10000
    }

  // https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
  // in-place implementation
  def floydWarshall(arr: Array[Array[Int]]): Unit =
    for {
      k <- 0 until arr.length
      i <- 0 until arr.length
      j <- 0 until arr.length
    } arr(i)(j) = math.min(arr(i)(j), arr(i)(k) + arr(k)(j))
}
