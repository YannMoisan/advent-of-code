package com.yannmoisan.util.grid

sealed abstract class Direction(
    val name: String,
    val delta: (Int, Int),
    val value: Int
)

sealed abstract case class Direction4(
    override val name: String,
    override val delta: (Int, Int),
    override val value: Int
) extends Direction(name, delta, value)

object Direction {
  object Up    extends Direction4("UP", (0, -1), 0)
  object Down  extends Direction4("DOWN", (0, 1), 1)
  object Left  extends Direction4("LEFT", (-1, 0), 2)
  object Right extends Direction4("RIGHT", (1, 0), 3)

  object NW extends Direction("NW", (-1, -1), 4)
  object NE extends Direction("NE", (1, -1), 5)
  object SW extends Direction("SW", (-1, 1), 6)
  object SE extends Direction("SE", (1, 1), 7)

  // clockwise
  def all4                               = Seq(Up, Right, Down, Left)
  def all8                               = all4 ++ Seq(NW, NE, SW, SE)
  def find(delta: (Int, Int)): Direction = all4.find(_.delta == delta).get
}
