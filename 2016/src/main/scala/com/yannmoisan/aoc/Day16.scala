package com.yannmoisan.aoc

object Day16 extends Puzzle {

  def dragonCurve(s: String) : String = s + "0" + s.reverse.map(c => if (c == '0') '1' else '0')

  // no foldWhile => use a stream or recursion
  def loop1(s: String, l: Int) : String = Rec.loop(s)(dragonCurve, _.length >= l)

  def checksum(s: String) = s.grouped(2).map(s => if (s(0) == s(1)) '1' else '0').mkString

  def loop2(s: String) : String = Rec.loop(s)(checksum, _.length % 2 != 0)

  override def part1: (Seq[String]) => String = { lines =>
    val data = loop1("11110010111001001", 272).take(272)
    loop2(data)
  }

  override def part2: (Seq[String]) => String = { lines =>
    val data = loop1("11110010111001001", 35651584).take(35651584)
    loop2(data)
  }
}
