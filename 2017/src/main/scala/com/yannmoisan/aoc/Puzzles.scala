package com.yannmoisan.aoc

trait Puzzle[O1, O2] {
  type I

  def input: I

  def part1: I => O1
  def part2: I => O2

  protected def lines = {
    val is = this.getClass.getClassLoader.getResourceAsStream(filename)
    io.Source.fromInputStream(is).getLines()
  }

  private def filename = {
    val clazz = this.getClass.getSimpleName
    val day = clazz.substring(3, clazz.length - 1)
    s"input$day"
  }
}

trait MultiPuzzle[O1, O2] extends Puzzle[O1, O2] {
  type I = Iterator[String]
  override def input = lines
}

trait SinglePuzzle[O1, O2] extends Puzzle[O1, O2] {
  type I = String
  override def input = lines.next()
}

object Puzzles extends App {
  val puzzle = Day10

  println(runPart1(puzzle))
  println(runPart2(puzzle))

  def runPart1[O1, O2](p: Puzzle[O1, O2]): O1 = p.part1(p.input)
  def runPart2[O1, O2](p: Puzzle[O1, O2]): O2 = p.part2(p.input)
}
