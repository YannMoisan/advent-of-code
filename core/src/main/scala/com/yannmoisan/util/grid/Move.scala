package com.yannmoisan.util.grid

trait Move {
  def move(index: Int, dir: DirectionWithIndex): Option[Int]
}

abstract class AbsMove(val dim: Dimension) extends Move

abstract class StandardMove(override val dim: Dimension) extends AbsMove(dim) {
  override def move(index: Int, dir: DirectionWithIndex): Option[Int] = {
    val pos = dim.positions(index)
    val dst = Pos(pos.x + dir.dx, pos.y + dir.dy)(dim)
    Option.when(isInRange(dst))(dst.index)
  }

  private def isInRange(p: Pos): Boolean =
    p.x >= 0 && p.x < dim.width && p.y >= 0 && p.y < dim.height

}

abstract class TorusShapedMove(override val dim: Dimension) extends AbsMove(dim) {
  override def move(index: Int, dir: DirectionWithIndex): Option[Int] = {
    val pos = dim.positions(index)
    val dst = Pos(
      (pos.x + dir.dx + dim.width)  % dim.width,
      (pos.y + dir.dy + dim.height) % dim.height
    )(dim)
    Some(dst.index)
  }
}

class PrecomputedMove(underlying: AbsMove) extends Move {
  override def move(index: Int, dir: DirectionWithIndex): Option[Int] = {
    val cacheIndex = dir.value + 8 * index
    cache(cacheIndex)
  }

  private val cache: Array[Option[Int]] = {
    val arr =
      Array.ofDim[Option[Int]](8 * underlying.dim.width * underlying.dim.height)
    for {
      dir <- Direction8.all
      p   <- underlying.dim.positions
    } {
      arr(dir.value + 8 * p.index) = underlying.move(p.index, dir)
    }
    arr
  }
}

object Test extends App {
  val classic = new PrecomputedMove(new StandardMove(Dimension(2, 3)) {})

  println(classic.move(0, Direction4.Up))
  println(classic.move(0, Direction4.Down))
  println(classic.move(0, Direction4.Left))
  println(classic.move(0, Direction4.Right))

  val torus = new PrecomputedMove(new TorusShapedMove(Dimension(2, 3)) {})

  println(torus.move(0, Direction4.Up))
  println(torus.move(0, Direction4.Down))
  println(torus.move(0, Direction4.Left))
  println(torus.move(0, Direction4.Right))

}
