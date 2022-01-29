import math.min
import math.max

object Day22 extends MultiPuzzle[Int, Long] with App {
  final case class RebootStep(isOn: Boolean, cube: Cube)
  final case class Cube(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) {
    def size: Long =
      (x2.toLong - x1.toLong + 1) * (y2.toLong - y1.toLong + 1) * (z2.toLong - z1.toLong + 1)
  }

  override def part1(input: Iterator[String]): Int = {
    val steps = input.map(parse)
    val cubes = steps.foldLeft(Set.empty[(Int, Int, Int)]) {
      case (acc, step) =>
        if (step.isOn)
          acc ++ toSet(step)
        else
          acc -- toSet(step)
    }
    cubes.size
  }

  override def part2(input: Iterator[String]): Long = {
    val steps = input.map(parse)
    var cubes = List.empty[Cube]

    steps.foreach { step =>
      val cube = step.cube

      cubes = cubes.flatMap { elem =>
        val cubes2 = new collection.mutable.ListBuffer[Cube]()
        if (!notIntersect(cube, elem)) {
          if (cube.x1 > elem.x1) {
            val _ = cubes2.append(Cube(elem.x1, cube.x1 - 1, elem.y1, elem.y2, elem.z1, elem.z2))
          }
          if (cube.x2 < elem.x2) {
            val _ = cubes2.append(Cube(cube.x2 + 1, elem.x2, elem.y1, elem.y2, elem.z1, elem.z2))
          }
          if (cube.y1 > elem.y1) {
            val _ = cubes2.append(
              Cube(
                max(elem.x1, cube.x1),
                min(elem.x2, cube.x2),
                elem.y1,
                cube.y1 - 1,
                elem.z1,
                elem.z2
              )
            )
          }
          if (cube.y2 < elem.y2) {
            val _ = cubes2.append(
              Cube(
                max(elem.x1, cube.x1),
                min(elem.x2, cube.x2),
                cube.y2 + 1,
                elem.y2,
                elem.z1,
                elem.z2
              )
            )
          }
          if (cube.z1 > elem.z1) {
            val _ = cubes2.append(
              Cube(
                max(elem.x1, cube.x1),
                min(elem.x2, cube.x2),
                max(elem.y1, cube.y1),
                min(elem.y2, cube.y2),
                elem.z1,
                cube.z1 - 1
              )
            )
          }
          if (cube.z2 < elem.z2) {
            val _ = cubes2.append(
              Cube(
                max(elem.x1, cube.x1),
                min(elem.x2, cube.x2),
                max(elem.y1, cube.y1),
                min(elem.y2, cube.y2),
                cube.z2 + 1,
                elem.z2
              )
            )
          }
          //
        } else {
          val _ = cubes2.append(elem)
        }
        cubes2.toList
      }
      //val _ = cubes.addAll(cubes2)
      if (step.isOn) {
        cubes =
          Cube(
            min(cube.x1, cube.x2),
            max(cube.x1, cube.x2),
            min(cube.y1, cube.y2),
            max(cube.y1, cube.y2),
            min(cube.z1, cube.z2),
            max(cube.z1, cube.z2)
          ) :: cubes
      }
    }
    cubes.map(_.size).sum

  }

  def parse(line: String): RebootStep = {
    val s"$b x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" = line
    RebootStep(b == "on", Cube(x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt))
  }

  def toSet(pl: RebootStep): Set[(Int, Int, Int)] =
    if (pl.cube.x1 > 50 || pl.cube.x2 < -50 || pl.cube.y1 > 50 || pl.cube.y2 < -50 || pl.cube.z1 > 50 || pl.cube.z2 < -50) {
      Set.empty
    } else {
      (for {
        x <- pl.cube.x1 to pl.cube.x2
        y <- pl.cube.y1 to pl.cube.y2
        z <- pl.cube.z1 to pl.cube.z2
        if x >= -50 && x <= 50 && y >= -50 && y <= 50 && z >= -50 && z <= 50
      } yield (x, y, z)).toSet
    }

  def notIntersect(cube1: Cube, cube2: Cube): Boolean =
    cube1.x1 > cube2.x2 || cube1.x2 < cube2.x1 ||
      cube1.y1 > cube2.y2 || cube1.y2 < cube2.y1 ||
      cube1.z1 > cube2.z2 || cube1.z2 < cube2.z1
}
