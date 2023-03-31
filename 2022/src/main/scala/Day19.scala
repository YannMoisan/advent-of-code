import com.yannmoisan.util.graph.BFS

object Day19 extends MultiPuzzle[Int, Int] {
//  // Blueprint 1:
//  //  Each ore robot costs 4 ore.
//  //  Each clay robot costs 2 ore.
//  //  Each obsidian robot costs 3 ore and 14 clay.
//  //  Each geode robot costs 2 ore and 7 obsidian.
//  val blueprint1: Seq[Array[Int]] = Vector(
//    Array(4, 0, 0, 0),  // ore robot
//    Array(2, 0, 0, 0),  // clay robot
//    Array(3, 14, 0, 0), // obsidian robot
//    Array(2, 0, 7, 0)   // geode robot
//  )

  case class Blueprint(id: Int, costs: Vector[Array[Int]])

  case class State(minute: Int, resources: Vector[Int], robots: Vector[Int])

  def next(bp: Blueprint)(s: State): Seq[State] =
    if (s.minute == 25) Seq.empty
    else {
      val updatedResources = Vector.tabulate(4)(i => s.resources(i) + s.robots(i))
      State(
        s.minute + 1,
        updatedResources,
        s.robots
      ) +: (0 to 3)
        .filter(i => s.minute != 24 && (0 to 3).forall(j => bp.costs(i)(j) <= s.resources(j))).map {
          i =>
            State(
              s.minute + 1,
              Vector.tabulate(4)(k => updatedResources(k) - bp.costs(i)(k)),
              s.robots.updated(i, s.robots(i) + 1)
            )
        }
    }

  override def part1(input: Iterator[String]): Int = {
    val blueprints = input.map {
      case s"Blueprint $id: Each ore robot costs $a ore. Each clay robot costs $b ore. Each obsidian robot costs $c ore and $d clay. Each geode robot costs $e ore and $f obsidian." =>
        Blueprint(
          id.toInt,
          Vector(
            Array(a.toInt, 0, 0, 0),       // ore robot
            Array(b.toInt, 0, 0, 0),       // clay robot
            Array(c.toInt, d.toInt, 0, 0), // obsidian robot
            Array(e.toInt, 0, f.toInt, 0)  // geode robot
          )
        )
    }
    blueprints.map(qualityLevel).sum
  }

  def qualityLevel(blueprint: Blueprint): Int = {
    println(blueprint.id)
    val init = State(1, Vector(0, 0, 0, 0), Vector(1, 0, 0, 0))
    val geodes = BFS
      .breadth_first_traverse_no_path_it(init, next(blueprint)).filter(_.minute == 24).maxBy(
        _.resources(3)
      ).resources(3)
    geodes * blueprint.id
  }

  override def part2(input: Iterator[String]): Int = 42
}
