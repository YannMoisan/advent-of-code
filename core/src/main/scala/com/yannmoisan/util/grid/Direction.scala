package com.yannmoisan.util.grid

sealed abstract case class Direction(
    name: String,
    delta: (Int, Int),
    value: Int
)

object Direction {
  object Up extends Direction("UP", (0, -1), 0)
  object Down extends Direction("DOWN", (0, 1), 1)
  object Left extends Direction("LEFT", (-1, 0), 2)
  object Right extends Direction("RIGHT", (1, 0), 3)

  def all = Seq(Up, Down, Left, Right)
  def find(delta: (Int, Int)): Direction = all.find(_.delta == delta).get
}

object Direction8 {
  val all = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
}
