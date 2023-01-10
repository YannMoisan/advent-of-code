package com

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, State, Threads, Warmup}

import java.util.concurrent.TimeUnit
import scala.collection.mutable

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
class BullsAndCowsEvaluateBench {
  var sol = "1273456"
  var guess = "3467232"

  @Benchmark
  def evaluateMap(): (Int, Int) = {
    // Find bulls
    var bulls = 0
    var cows = 0
    // populate the state for the algo
    val state = mutable.Map[Char, Int]()
    sol.foreach { c => state.update(c, 1 + state.getOrElse(c, 0)) }

    sol.indices.foreach { i =>
      if (sol(i) == guess(i)) {
        bulls += 1
        state.update(guess(i), state(guess(i)) - 1)
      }
    }

    sol.indices.foreach { i =>
      if (sol(i) != guess(i)) { // not a bull, might be a cow
        if (state.get(guess(i)).exists(_ > 0)) {
          cows += 1
          state.update(guess(i), state(guess(i)) - 1)
        }
      }
    }
    (bulls, cows)
  }

  @Benchmark
  def evaluateArrayWithoutForeach(): (Int, Int) = {
    // Find bulls
    var bulls = 0
    var cows = 0
    // populate the state for the algo
    val state = Array.ofDim[Int](10)
    //mutable.Map[Char, Int]()
    sol.foreach { c =>
      val index = c - '0'
      state(index) = state(index) + 1
    }

    val len = sol.length

    var i = 0
    while (i < len) {
      if (sol(i) == guess(i)) {
        val index = guess.apply(i) - '0'
        bulls += 1
        state(index) = state(index) - 1
      }
      i += 1
    }

    i = 0
    while (i < len) {
      if (sol(i) != guess(i)) { // not a bull, might be a cow
        val index = guess(i) - '0'
        if (state(index) > 0) {
          cows += 1
          state(index) = state(index) - 1
        }
      }
      i += 1
    }
    (bulls, cows)
  }

  @Benchmark
  def evaluateArrayWithoutForeachCharAt(): (Int, Int) = {
    // Find bulls
    var bulls = 0
    var cows = 0
    // populate the state for the algo
    val state = Array.ofDim[Int](10)
    //mutable.Map[Char, Int]()
    sol.foreach { c =>
      val index = c - '0'
      state(index) = state(index) + 1
    }

    val len = sol.length

    var i = 0
    while (i < len) {
      if (sol.charAt(i) == guess.charAt(i)) {
        val index = guess.charAt(i) - '0'
        bulls += 1
        state(index) = state(index) - 1
      }
      i += 1
    }

    i = 0
    while (i < len) {
      if (sol.charAt(i) != guess.charAt(i)) { // not a bull, might be a cow
        val index = guess.charAt(i) - '0'
        if (state(index) > 0) {
          cows += 1
          state(index) = state(index) - 1
        }
      }
      i += 1
    }
    (bulls, cows)
  }

  @Benchmark
  def evaluateNoDups(): (Int, Int) = {
    // Find bulls
    var bulls = 0
    var cows = 0
    // populate the state for the algo
    val state = Array.ofDim[Int](10)
    //mutable.Map[Char, Int]()
    sol.foreach { c =>
      val index = c - '0'
      state(index) = state(index) + 1
    }

    val len = sol.length

    var i = 0
    while (i < len) {
      val index = guess.charAt(i) - '0'
      if (sol.charAt(i) == guess.charAt(i)) {
        bulls += 1
      } else if (state(index) > 0) {
        cows += 1
      }
      i+=1
    }
    (bulls, cows)
  }
}
