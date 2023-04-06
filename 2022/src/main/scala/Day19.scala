import com.yannmoisan.util.graph.BFS

object Day19 extends MultiPuzzle[Int, Int] {

  case class Blueprint(id: Int, costs: Vector[Array[Int]]) {
    val max = (0 to 3).map(resourceIndex => (0 to 3).map(costs(_)(resourceIndex)).max)
  }

  case class State(
      minute: Int,
      resources: Vector[Int],
      robots: Vector[Int],
      canBuildPrev: Array[Boolean]
  )

  def next(bp: Blueprint, maxTime: Int)(s: State): Seq[State] =
    if (s.minute == maxTime) Seq.empty
    else {
      val updatedResources = Vector.tabulate(4)(i => s.resources(i) + s.robots(i))
      // What are the type of robots that I can build if I wait enough time
      // and the count is below the max
      val canBuild: Array[Boolean] = Array(
        s.robots(0) < bp.max(0), // we produce ore
        s.robots(1) < bp.max(1), // we produce ore
        s.robots(2) < bp.max(2) && s
          .robots(1) > 0, // we need to produce clay     to build an obsidian robot
        s.robots(2) > 0   // we need to produce obsidian to build a  geode    robot
      )

      // Do we have enough resources to build this robot
      val canBuildNow = (0 to 3).map { robotId =>
        (0 to 3).forall(j => bp.costs(robotId)(j) <= s.resources(j))
      }.toArray

      // If we have enough resources to build all robot possible => ONLY build
      // Else build & wait
      val build: Seq[State] =
        (0 to 3)
          .filter(i =>
            s.minute != maxTime - 1 && canBuildNow(i) && canBuild(i) && !s.canBuildPrev(i)
          ).map(i =>
            State(
              s.minute + 1,
              Vector.tabulate(4)(k => updatedResources(k) - bp.costs(i)(k)),
              s.robots.updated(i, s.robots(i) + 1),
              Array(false, false, false, false)
            )
          )

      val wait = State(
        s.minute + 1,
        updatedResources,
        s.robots,
        canBuildNow
      )

      if (s.minute != maxTime - 1 && canBuildNow(3) && canBuild(3) && !s.canBuildPrev(3)) {
        Seq(build.last)
      } else {
        if (canBuild.count(_ == true) == canBuildNow.count(_ == true))
          build
        else
          wait +: build
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
    blueprints.map(bp => geodes(bp, 24) * bp.id).sum
  }

  def geodes(blueprint: Blueprint, maxTime: Int): Int = {
    val init = State(0, Vector(0, 0, 0, 0), Vector(1, 0, 0, 0), Array(false, false, false, false))
    BFS
      .breadth_first_traverse_no_path_it(init, next(blueprint, maxTime)).filter(_.minute == maxTime).maxBy(
        _.resources(3)
      ).resources(3)
  }

  override def part2(input: Iterator[String]): Int = {
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
    blueprints.take(3).map(bp => geodes(bp, 32)).product
  }
}
