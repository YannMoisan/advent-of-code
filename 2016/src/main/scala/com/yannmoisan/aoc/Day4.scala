package com.yannmoisan.aoc

object Day4 extends Puzzle {

  def shiftS(s: String, nb: Int) = s.replace("-", "").map(Rec.shiftN('a' to 'z', nb))

  def computeRoom2(s: String): (String, String) = {
    val room = """(.*)-(\d+)\[(.*)\]""".r
    s match {
      case room(name, id, checksum) =>
        (shiftS(name, id.toInt), id)
    }
  }

  def computeRoom1(s: String): Int = {
    val room = """(.*)-(\d+)\[(.*)\]""".r
    s match {
      case room(name, id, checksum) =>
        val chk = name.
          replace("-", "").
          groupBy(identity).
          mapValues(_.length).
          toList.
          sortBy { case (c, i) => -(i * 100 - c.toInt) }.
          map(_._1).
          take(5).
          mkString("")
        if (checksum == chk) id.toInt else 0
      case _ => 0
    }
  }

  override def part1 = _.map(computeRoom1).sum

  override def part2 = _.map(computeRoom2).toMap.get("northpoleobjectstorage").get

}