package com

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.collection.mutable

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
class Day1Bench {
  var lines: Array[Int] = _

  @Benchmark
  def part2_stream: Int = {
    val frequencies = lines
    LazyList
      .continually(frequencies)
      .flatten
      .scanLeft((0, Set.empty[Int])) {
        case ((sum, visited), freq) => (sum + freq, visited + sum)
      }
      .find { case (sum, visited) => visited.contains(sum) }
      .get._1
  }

  @Benchmark
  def part2_iterator: Int = {
    val frequencies = lines
    Iterator
      .continually(frequencies)
      .flatten
      .scanLeft((0, Set.empty[Int])) {
        case ((sum, visited), freq) => (sum + freq, visited + sum)
      }
      .find { case (sum, visited) => visited.contains(sum) }
      .get._1
  }

  @Benchmark
  def part2_iterator_mutable: Int = {
    val frequencies = lines
    val it          = Iterator.continually(frequencies).flatten.scanLeft(0)(_ + _)
    val visited     = mutable.Set[Int]()
    while (it.hasNext) {
      val cur = it.next()
      if (visited.contains(cur)) return cur
      visited.add(cur)
    }
    -1
  }

  @Benchmark
  def part2_recursive: Int = {
    def loop(lines: Array[Int], idx: Int, sum: Int, visited: Set[Int]): Int = {
      val v    = lines(idx % lines.length)
      val sum2 = sum + v
      if (visited.contains(sum2)) sum2
      else loop(lines, idx + 1, sum2, visited + sum2)
    }

    loop(lines, 0, 0, Set.empty)
  }

  @Benchmark
  def part2_recursive_mutable: Int = {
    val visited = mutable.Set[Int]()
    def loop(lines: Array[Int], idx: Int, sum: Int): Int = {
      val v    = lines(idx % lines.length)
      val sum2 = sum + v
      if (visited.contains(sum2)) sum2
      else {
        visited.add(sum2)
        loop(lines, idx + 1, sum2)
      }
    }

    loop(lines, 0, 0)
  }

  @Benchmark
  def part2_imperative: Int = {
    var i                  = 0
    var sum                = 0
    val visited            = mutable.Set(0)
    var found: Option[Int] = None
    while (found.isEmpty) {
      sum += lines(i % lines.length).toInt
      if (visited.contains(sum)) { found = Some(sum) }
      visited += sum
      i += 1
    }
    found.get
  }

  @Setup
  def setup(): Unit = {
    val is = this.getClass.getClassLoader.getResourceAsStream("input1")
    lines = io.Source.fromInputStream(is).getLines().toArray.map(_.toInt)
    println("lenth=" + lines.length)
  }
}
