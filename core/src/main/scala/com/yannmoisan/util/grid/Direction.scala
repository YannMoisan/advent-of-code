package com.yannmoisan.util.grid

abstract class Direction(val dx: Int, val dy: Int, val value: Int)

sealed abstract case class Direction4(
    override val dx: Int,
    override val dy: Int,
    override val value: Int
) extends Direction(dx, dy, value)

sealed abstract case class Direction8(
    override val dx: Int,
    override val dy: Int,
    override val value: Int
) extends Direction(dx, dy, value)

object Direction4 {
  object Up    extends Direction4(0, -1, 0)
  object Down  extends Direction4(0, 1, 1)
  object Left  extends Direction4(-1, 0, 2)
  object Right extends Direction4(1, 0, 3)

  // clockwise
  def all: Seq[Direction4] = Seq(Up, Right, Down, Left)

  def find(delta: (Int, Int)): Direction = all.find(d => delta._1 == d.dx && delta._2 == d.dy).get
}

object Direction8 {
  object Up    extends Direction8(0, -1, 0)
  object Down  extends Direction8(0, 1, 1)
  object Left  extends Direction8(-1, 0, 2)
  object Right extends Direction8(1, 0, 3)

  object NW extends Direction8(-1, -1, 4)
  object NE extends Direction8(1, -1, 5)
  object SW extends Direction8(-1, 1, 6)
  object SE extends Direction8(1, 1, 7)

  def all: Seq[Direction8] = Seq(Up, Down, Left, Right, NW, NE, SW, SE)
}
