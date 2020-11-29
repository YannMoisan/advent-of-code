package com.yannmoisan.aoc
import scala.collection.immutable.IndexedSeq

object Day24 extends Puzzle {

  type Grid = Seq[String]

  case class Pos(x: Int, y: Int)

  def possibleMoves(p: Pos): Seq[Pos] = Seq(
    Pos(p.x - 1, p.y),
    Pos(p.x + 1, p.y),
    Pos(p.x, p.y - 1),
    Pos(p.x, p.y + 1)
  )

  def validPos(p: Pos, g: Grid) =
    p.x >= 0 &&
      p.x < g(0).length &&
      p.y >= 0 &&
      p.y < g.size &&
      g(p.y)(p.x) != '#'

  def moves: State => Seq[State] = s => possibleMoves(s.p).filter(p => validPos(p, s.grid)).map(p => State(s.grid, p))

  case class State(grid: Grid, p: Pos)

  // no need to convert char to int
  def findNumbers(grid: Grid) : Seq[(Int, Pos)] = for {
    x <- 0 until grid(0).length
    y <- 0 until grid.size
    if (grid(y)(x)!= '.' && grid(y)(x) != '#')
  } yield (grid(y)(x).toString.toInt, Pos(x,y))

  def dist(grid: Grid, p1: Pos, p2: Pos) : Int = {
    val init = State(grid, p1)
    val nodes = BFS.breadth_first_traverse(init, moves)
    nodes.find(_._1.p == p2).get._2.size - 1
  }

  type Path = Seq[(Pos, Pos)]

  def pathDist(grid: Grid, path: Path, dists: Dists) : Int = path.map(dists).sum

  def paths(mapping: Map[Int, Pos], shouldReturn: Boolean): Seq[Path] = {
    val startBy0 = (0 to 7)
      .map(mapping)
      .permutations
      .filter(_(0) == mapping(0))

    val withReturn = if (shouldReturn) startBy0.map(l => l :+ mapping(0)) else startBy0

    withReturn.map(_.sliding(2))
              .map(_.map(l => (l(0), l(1))))
              .map(_.toList).toList
  }

  type Dists = Map[(Pos, Pos), Int]

  def computeDists(grid: Grid, mapping: Map[Int, Pos]) : Dists = {
    val combinations : Iterator[IndexedSeq[Pos]] = (0 to 7).map(mapping).combinations(2)
    val paired: Iterator[(Pos, Pos)] = combinations.map(l => (l(0), l(1)))
    val ds: Map[(Pos, Pos), Int] = paired.map(t => (t, dist(grid, t._1, t._2))).toMap
    ds ++ ds.toList.map{ case (t, d) => ((t._2, t._1), d)}.toMap
  }

  def part(shouldReturn: Boolean) = { lines:Seq[String] =>
    val grid = lines
    val numbers: Map[Int, Pos] = findNumbers(grid).toMap
    val dists = computeDists(grid, numbers)
    paths(numbers, shouldReturn).map(p => pathDist(grid, p, dists)).min
  }

  override def part1 =  part(false)

  override def part2 = part(true)
}
