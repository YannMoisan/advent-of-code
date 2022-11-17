package com.yannmoisan.aoc

import java.security.MessageDigest

import sun.security.provider.MD5

// http://stackoverflow.com/questions/5992778/computing-the-md5-hash-of-a-string-in-scala
// http://stackoverflow.com/questions/38855843/scala-one-liner-to-generate-md5-hash-from-string
object Day5 extends Puzzle {

  override def part1: (Seq[String]) => String = _ => ""
//    Stream.from(1)
//      .map(i => MD5.md5(s"uqwqemis$i"))
//      .filter(_.startsWith("00000"))
//      .map(_ (5))
//      .take(8)
//      .mkString("")

  override def part2: (Seq[String]) => String = part1
}
