import scala.collection.mutable

object Day12 extends MultiPuzzle[Int, Long] {
  override def part1: Iterator[String] => Int = { _ =>
    val vel0 = Position(0, 0, 0)

//    val moons = Array(
//      Moon(Position(-1, 0, 2), vel0),
//      Moon(Position(2, -10, -7), vel0),
//      Moon(Position(4, -8, 8), vel0),
//      Moon(Position(3, 5, -1), vel0)
//    )

//    <x=0, y=4, z=0>
//    <x=-10, y=-6, z=-14>
//    <x=9, y=-16, z=-3>
//    <x=6, y=-1, z=2>

    val moons = Array(
      Moon(Position(0, 4, 0), vel0),
      Moon(Position(-10, -6, -14), vel0),
      Moon(Position(9, -16, -3), vel0),
      Moon(Position(6, -1, 2), vel0)
    )

    val end: Array[Moon] = (0 until 1000).foldLeft(moons) {
      case (acc, _) =>
        //println(s"step $i")
        //acc.foreach(println)
        move(acc)
    }

    end.map(_.energy).foreach(println)
    end.map(_.energy).sum
  }

  override def part2: Iterator[String] => Long = { _ =>
    val vel0 = Position(0, 0, 0)

//    val moons = Array(
//      Moon(Position(-1, 0, 2), vel0),
//      Moon(Position(2, -10, -7), vel0),
//      Moon(Position(4, -8, 8), vel0),
//      Moon(Position(3, 5, -1), vel0)
//    )

    //    <x=0, y=4, z=0>
    //    <x=-10, y=-6, z=-14>
    //    <x=9, y=-16, z=-3>
    //    <x=6, y=-1, z=2>

    val moons = Array(
      Moon(Position(0, 4, 0), vel0),
      Moon(Position(-10, -6, -14), vel0),
      Moon(Position(9, -16, -3), vel0),
      Moon(Position(6, -1, 2), vel0)
    )

    var i                                 = 0L
    var continue                          = true
    var cur: Array[Moon]                  = moons
    val history: mutable.Set[Array[Moon]] = mutable.Set(moons)
    while (continue) {
      i += 1
      if (i % 1000000 == 0) {
        //println(s"min x=${history.minBy(_.minBy(_.pos.x).pos.x)}")
        //println(s"max x=${history.maxBy(_.maxBy(_.pos.x).pos.x)}")
        println(i)
      }

      val newMoons = move(cur)
      if (history.contains(newMoons)) continue = false
      else {
        cur = newMoons
        history.add(newMoons)
      }
    }

    i
//
//    println(i)
//
//    val end: Seq[Array[Moon]] = (0 until 1000).scanLeft(moons) {
//      case (acc, _) =>
//        //println(s"step $i")
//        //acc.foreach(println)
//        move(acc)
//    }
//
//    end.map(_.energy).foreach(println)
//    end.map(_.energy).sum
  }

  case class Position(x: Int, y: Int, z: Int) {
    val energy: Int = math.abs(x) + math.abs(y) + math.abs(z)
  }
  //case class Velocity(x: Int, y: Int, z: Int)

  case class Moon(pos: Position, vel: Position) {
    val energy: Int = {
      //println(s"${pos.energy} * ${vel.energy}")

      pos.energy * vel.energy
    }
  }

  def add(pos: Position, vel: Position): Position = {
    Position(pos.x + vel.x, pos.y + vel.y, pos.z + vel.z)
  }

  def gravity(moons: Array[Moon]): Array[Position] = {
    indexes.map { i =>
      val others = otherz(i)
      val dx     = sumBy(others, (j: Int) => g(moons(i).pos.x, moons(j).pos.x))
//      others.map { j =>
//        g(moons(i).pos.x, moons(j).pos.x)
//      }.sum

      //      val dx = others.map { j =>
//        g(moons(i).pos.x, moons(j).pos.x)
//      }.sum
      val dy = sumBy(others, (j: Int) => g(moons(i).pos.y, moons(j).pos.y))
//      val dy = others.map { j =>
//        g(moons(i).pos.y, moons(j).pos.y)
//      }.sum
      val dz = sumBy(others, (j: Int) => g(moons(i).pos.z, moons(j).pos.z))
//      val dz = others.map { j =>
//        g(moons(i).pos.z, moons(j).pos.z)
//      }.sum
      Position(dx, dy, dz)
    }
  }

  def sumBy[A](a: Array[A], f: A => Int): Int = {
    var i   = 0
    var sum = 0
    while (i < a.length) {
      sum += f(a(i))
      i += 1
    }
    sum
  }

  def g(a: Int, b: Int): Int = {
    if (a == b) 0 else if (a > b) -1 else 1
  }

  val indexes = Array(0, 1, 2, 3)
  val otherz: Array[Array[Int]] = indexes.map { i =>
    indexes.filter(_ != i)
  }

  def move(moons: Array[Moon]): Array[Moon] = {
    indexes.map { i =>
      val gravities = gravity(moons)
      val newVel    = add(moons(i).vel, gravities(i))
      Moon(add(moons(i).pos, newVel), newVel)
    }
  }

}
