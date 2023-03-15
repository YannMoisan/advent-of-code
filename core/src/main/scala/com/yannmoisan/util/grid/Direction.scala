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

  object NW extends Direction("NW", (-1, -1), 4)
  object NE extends Direction("NE", (1, -1), 5)
  object SW extends Direction("SW", (-1, 1), 6)
  object SE extends Direction("SE", (1, 1), 7)

  def all4 = Seq(Up, Down, Left, Right)
  def all8 = all4 ++ Seq(NW, NE, SW, SE)
  def find(delta: (Int, Int)): Direction = all4.find(_.delta == delta).get
}
