package com.yannmoisan.util.grid

trait Move {
  def move(index: Int, dir: Direction): Option[Int]
}

abstract class AbsMove(val dim: Dimension) extends Move

abstract class StandardMove(override val dim: Dimension) extends AbsMove(dim) {
  override def move(index: Int, dir: Direction): Option[Int] = {
    val pos = dim.allPos(index)
    val dst = Pos(pos.x + dir.delta._1, pos.y + dir.delta._2)(dim)
    Option.when(isInRange(dst))(dst.index)
  }

  private def isInRange(p: Pos): Boolean =
    p.x >= 0 && p.x < dim.width && p.y >= 0 && p.y < dim.height

}

abstract class TorusShapedMove(override val dim: Dimension)
    extends AbsMove(dim) {
  override def move(index: Int, dir: Direction): Option[Int] = {
    val pos = dim.allPos(index)
    val dst = Pos(
      (pos.x + dir.delta._1 + dim.width) % dim.width,
      (pos.y + dir.delta._2 + dim.height) % dim.height
    )(dim)
    Some(dst.index)
  }
}

class PrecomputedMove(underlying: AbsMove) extends Move {
  override def move(index: Int, dir: Direction): Option[Int] = {
    val cacheIndex = dir.value + 8 * index
    cache(cacheIndex)
  }

  private val cache: Array[Option[Int]] = {
    val arr =
      Array.ofDim[Option[Int]](8 * underlying.dim.width * underlying.dim.height)
    for {
      dir <- Direction.all8
      p <- underlying.dim.allPos
    } {
      arr(dir.value + 8 * p.index) = underlying.move(p.index, dir)
    }
    arr
  }
}

object Test extends App {
  val classic = new PrecomputedMove(new StandardMove(Dimension(2, 3)) {})

  println(classic.move(0, Direction.Up))
  println(classic.move(0, Direction.Down))
  println(classic.move(0, Direction.Left))
  println(classic.move(0, Direction.Right))

  val torus = new PrecomputedMove(new TorusShapedMove(Dimension(2, 3)) {})

  println(torus.move(0, Direction.Up))
  println(torus.move(0, Direction.Down))
  println(torus.move(0, Direction.Left))
  println(torus.move(0, Direction.Right))

}
