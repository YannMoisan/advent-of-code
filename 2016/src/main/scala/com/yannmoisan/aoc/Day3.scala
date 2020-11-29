package com.yannmoisan.aoc

object Day3 extends Puzzle {

  def parse : Seq[String] => Seq[Array[Int]] =
    _.map(_.split("\\s+").tail.map(_.toInt))

  def countTriangles(s: Seq[Seq[Int]]) =
    s.map(_.sortBy(identity))
      .filter(a => a(0) + a(1) > a(2))
      .size

  def part1 = lines => {
    val triangles = parse(lines).map(_.toList)
    countTriangles(triangles)
  }

  def part2 = lines => {
    val triangles = parse(lines).transpose.flatten.grouped(3).toList
    countTriangles(triangles)
  }
}
