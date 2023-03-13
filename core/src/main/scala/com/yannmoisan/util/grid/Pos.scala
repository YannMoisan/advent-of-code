package com.yannmoisan.util.grid

abstract case class Pos(x: Int, y: Int, index: Int)

object Pos {
  def apply(x: Int, y: Int)(dim: Dimension): Pos = {
    new Pos(x, y, x + y * dim.width) {}
  }
  def direction(current: Pos, next: Pos): Direction = {
    val delta = (next.x - current.x, next.y - current.y)
    Direction.find(delta)
  }
}
