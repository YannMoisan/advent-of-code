package com.yannmoisan.util.grid

case class Pos(x: Int, y: Int)

object Pos {
  def direction(current: Pos, next: Pos): Direction = {
    val delta = (next.x - current.x, next.y - current.y)
    Direction4.find(delta)
  }
}
