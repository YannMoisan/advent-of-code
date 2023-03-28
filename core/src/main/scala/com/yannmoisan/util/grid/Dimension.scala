package com.yannmoisan.util.grid

import scala.collection.mutable

sealed abstract case class Dimension(width: Int, height: Int) {
  val size = width * height
  // for perf reason (cpu cache), order matters here
  private[grid] val positions = (for {
    y <- 0 until height
    x <- 0 until width
  } yield Pos(x, y)).toArray

  val indices     = 0 until width * height
  def pos(i: Int) = positions(i)

  def index(pos: Pos): Int = pos.x + pos.y * width

  private val neighbors4_ =
    new PrecomputedNeighbors(new StandardMove(this) {}, Direction4.all)
  def neighbors4(p: Int): Array[Int] = neighbors4_.neighbors(p)

  private val neighbors4Torus_ =
    new PrecomputedNeighbors(new TorusShapedMove(this) {}, Direction4.all)
  def neighbors4Torus(p: Int): Array[Int] = neighbors4Torus_.neighbors(p)

  private val neighbors8_ =
    new PrecomputedNeighbors(new StandardMove(this) {}, Direction8.all)
  def neighbors8(p: Int): Array[Int] = neighbors8_.neighbors(p)

  private val move_                                          = new PrecomputedMove(new TorusShapedMove(this) {})
  def move(index: Int, dir: DirectionWithIndex): Option[Int] = move_.move(index, dir)

  private val moveS_                                          = new PrecomputedMove(new StandardMove(this) {})
  def moveS(index: Int, dir: DirectionWithIndex): Option[Int] = moveS_.move(index, dir)
}

// Use only in a single-threaded context
object Dimension {
  private val cache = mutable.Map[(Int, Int), Dimension]()

  def apply(width: Int, height: Int): Dimension =
    cache.getOrElseUpdate((width, height), new Dimension(width, height) {})
}
