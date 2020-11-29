package com.yannmoisan.aoc

trait Puzzle {
  def part1 : Seq[String] => Any
  def part2 : Seq[String] => Any
  def filename = {
    val clazz = this.getClass.getSimpleName
    val day = clazz.substring(3, clazz.length - 1)
    s"input$day"
  }
}

object Puzzles {
  val puzzle = Day10

  def main(args: Array[String]) {
    println(part1(puzzle))
    println(part2(puzzle))
  }

  def part1(p: Puzzle) : Any = compute2(p.filename)(p.part1)
  def part2(p: Puzzle) : Any = compute2(p.filename)(p.part2)

  def compute2[A](filename: String)(f: Seq[String] => A) = {
    val is = this.getClass.getClassLoader.getResourceAsStream(filename)
    val lines = io.Source.fromInputStream(is).getLines().toList
//    val lines2 = Seq("value 5 goes to bot 2",
//    "bot 2 gives low to bot 1 and high to bot 0",
//    "value 3 goes to bot 1",
//    "bot 1 gives low to output 1 and high to bot 0",
//    "bot 0 gives low to output 2 and high to output 0",
//    "value 2 goes to bot 2")
//    f(lines2)
    f(lines)
  }
}
