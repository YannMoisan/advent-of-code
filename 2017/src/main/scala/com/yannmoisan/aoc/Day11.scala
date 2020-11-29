package com.yannmoisan.aoc

object Day11 extends SinglePuzzle[Int, Int] {
  override def part1 = { line =>
    val m = Map("ne" -> 0, "n" -> 0, "nw" -> 0)
    val allMaps: Array[Map[String, Int]] = line.split(",").scanLeft(m) {
      case (m, dir) =>
        dir match {
          case "ne" => m.updated("ne", m("ne") + 1)
          case "n"  => m.updated("n", m("n") + 1)
          case "nw" => m.updated("nw", m("nw") + 1)
          case "se" => m.updated("nw", m("nw") - 1)
          case "s"  => m.updated("n", m("n") - 1)
          case "sw" => m.updated("ne", m("ne") - 1)
        }
    }

    allMaps.map { m =>
      if (m("ne") > 0 && m("n") < 0 || m("ne") < 0 && m("n") > 0)
        math.abs(m("nw")) + math.max(math.abs(m("ne")), math.abs(m("n")))
      else if (m("nw") > 0 && m("n") < 0 || m("nw") < 0 && m("n") > 0)
        math.abs(m("ne")) + math.max(math.abs(m("nw")), math.abs(m("nw")))
      else
        math.abs(m("ne")) + math.abs(m("n") + math.abs(m("nw")))
    }.max
  }

  override def part2 = { line =>
    0
  }
}
