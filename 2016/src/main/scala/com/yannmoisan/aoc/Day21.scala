package com.yannmoisan.aoc

object Day21 extends Puzzle {

  val reversePos = """reverse positions (\d) through (\d)""".r
  val rotatePos = """rotate based on position of letter (\w)""".r
  val rotateRight = """rotate right (\w) steps?""".r
  val rotateLeft = """rotate left (\w) steps?""".r
  val swapPos = """swap position (\d) with position (\d)""".r
  val swapLetter = """swap letter (\w) with letter (\w)""".r
  val movePos = """move position (\d) to position (\d)""".r

  def next(instruction: String, s: String): String = instruction match {
    case reversePos(p1, p2) => reverse(s, p1.toInt, p2.toInt)
    case rotatePos(p1) => rotatePos(s, p1)
    case rotateLeft(p1) => rotateLeft(s, p1.toInt)
    case rotateRight(p1) => rotateRight(s, p1.toInt)
    case swapPos(p1, p2) => swap(s, p1.toInt, p2.toInt)
    case swapLetter(p1, p2) => swap(s, s.indexOf(p1), s.indexOf(p2))
    case movePos(p1, p2) => movePos(s, p1.toInt, p2.toInt)
  }

  def movePos(s: String, p1: Int, p2: Int): String = {
    val c: Char = s(p1)
    val tmp = s.replace(c.toString, "")
    val (b, e) = tmp.splitAt(p2)
    b + c + e
  }

  def swap(s: String, p1: Int, p2: Int): String =
    s.updated(p1, s(p2)).updated(p2, s(p1))

  def rotatePos(s: String, p1: String) = rotateRight(s, ((if (s.indexOf(p1) >= 4) +1 else 0) + s.indexOf(p1) + 1) % s.length)

  def rotateLeft(s: String, n: Int) = splitAndReverse(s, n)

  def rotateRight(s: String, n: Int) = splitAndReverse(s, s.length - n)

  def splitAndReverse(s: String, i: Int): String = {
    val (b, e) = s.splitAt(i)
    e + b
  }

  def reverse(s: String, p1: Int, p2: Int) =
    s.patch(p1, s.substring(p1, p2 + 1).reverse, p2 - p1 + 1)

  def undoNext(instruction: String, s: String): String = instruction match {
    case reversePos(p1, p2) => reverse(s, p1.toInt, p2.toInt) //nothing
    case rotatePos(p1) => undoRotatePos(s, p1)
    case rotateLeft(p1) => rotateRight(s, p1.toInt) // left => right
    case rotateRight(p1) => rotateLeft(s, p1.toInt) // right => left
    case swapPos(p1, p2) => swap(s, p1.toInt, p2.toInt) // nothing
    case swapLetter(p1, p2) => swap(s, s.indexOf(p1), s.indexOf(p2)) // nothing
    case movePos(p1, p2) => movePos(s, p2.toInt, p1.toInt) // rev order
  }

  def undoRotatePos(s: String, p1: String) = {
    s.indexOf(p1) match {
      case 0 => rotateLeft(s, 1)
      case 6 => s
      case 4 => rotateRight(s, 1)
      case 2 => rotateRight(s, 2)
      case 1 => rotateLeft(s, 1)
      case 3 => rotateLeft(s, 2)
      case 5 => rotateLeft(s, 3)
      case 7 => rotateLeft(s, 4)
    }
  }

  override def part1: (Seq[String]) => String = { lines =>
    lines.foldLeft("abcdefgh") { case (acc, i) => next(i, acc) }
  }

  override def part2: (Seq[String]) => String = { lines =>
    lines.reverse.foldLeft("fbgdceah") { case (acc, i) => undoNext(i, acc) }
  }
}
