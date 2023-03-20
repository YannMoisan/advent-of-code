package com

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
class Lambda {
  val arr1: Array[Int]        = Array.fill(100000)(Random.nextInt())
  val arr2: Array[(Int, Int)] = arr1.map(i => (i, i))

  val lambda1: Int => Boolean          = i => i % 2 == 0
  val lambda2: ((Int, Int)) => Boolean = i => i._1 % 2 == 0
  def method1(i: Int)                  = i % 2 == 0
  def method2(t: (Int, Int))           = t._1 % 2 == 0

  @Benchmark
  def b_1lambda: Int =
    arr1.count(lambda1)

  @Benchmark
  def b_1method: Int = {
    var c = 0
    arr1.foreach { i =>
      if (method1(i))
        c += 1
    }
    c
  }

  @Benchmark
  def b_1while: Int = {
    var c = 0
    var i = 0
    while (i < arr1.length) {
      if (method1(arr1(i)))
        c += 1
      i += 1
    }
    c
  }

  @Benchmark
  def b_2lambda: Int =
    arr2.count(lambda2)

  @Benchmark
  def b_2method: Int = {
    var c = 0
    arr2.foreach { i =>
      if (method2(i))
        c += 1
    }
    c
  }

  @Benchmark
  def b_2while: Int = {
    var c = 0
    var i = 0
    while (i < arr2.length) {
      if (method2(arr2(i)))
        c += 1
      i += 1
    }
    c
  }

}
