package com.yannmoisan.util.geo

import com.yannmoisan.util.grid.Direction

case class Position(x: Int, y: Int)

object Position {
  def move(pos: Position, dir: Direction): Position =
    Position(pos.x + dir.delta._1, pos.y + dir.delta._2)

  def dist(a: Position, b: Position): Double =
    math.sqrt(dist2(a, b).toDouble)

  def dist2(a: Position, b: Position): Int = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    dx * dx + dy * dy
  }

  def manhattan(a: Position, b: Position): Int =
    math.abs(a.x - b.x) + math.abs(a.y - b.y)

}
