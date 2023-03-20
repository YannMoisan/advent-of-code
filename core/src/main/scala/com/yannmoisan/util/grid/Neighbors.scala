package com.yannmoisan.util.grid

trait Neighbors {
  def neighbors(index: Int): Array[Int]
}

class PrecomputedNeighbors(move: AbsMove, directions: Seq[Direction])
    extends Neighbors {
  override def neighbors(index: Int): Array[Int] = cache(index)

  private val cache: Array[Array[Int]] = {
    val arr = Array.ofDim[Array[Int]](move.dim.width * move.dim.height)
    move.dim.positions.foreach { p =>
      arr(p.index) =
        directions.map { dir => move.move(p.index, dir) }.flatten.toArray
    }
    arr
  }
}

object NeighborsApp extends App {
  val classic = new PrecomputedNeighbors(
    new StandardMove(Dimension(2, 3)) {},
    Direction4.all
  )
  println(classic.neighbors(0).mkString(","))

  val torus = new PrecomputedNeighbors(
    new TorusShapedMove(Dimension(2, 3)) {},
    Direction4.all
  )
  println(torus.neighbors(0).mkString(","))
}
