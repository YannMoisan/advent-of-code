package com

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, State, Threads, Warmup}

import java.util.concurrent.TimeUnit
import scala.collection.mutable

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
class BruteForceBench {

  @Benchmark
  def test(): String = {
    BruteForce.solve(List(Guess("123456", 3, 0)))
  }
}

case class Guess(guess: String, bulls: Int, cows: Int)

trait BullsAndCows {
  def solve(guesses: List[Guess]) : String
}

object BruteForce extends BullsAndCows {
  val candidates = Combinatorics
    .permutations2(('0' to '9').toList, 6)
    .map(xs => new String(xs.toArray))
    .filter(_.head != '0')

  override def solve(guesses: List[Guess]): String = {
    //println(candidates.size)
    candidates.find { candidate =>
      guesses.forall { guess => evaluate(candidate, guess.guess) == ((guess.bulls, guess.cows)) }
    }.get
  }

  private def evaluate(sol: String, guess: String): (Int, Int) = {
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
}

object Combinatorics extends App {

  def permutations2[A](l: List[A], k: Int): List[List[A]] = {
    // dfs
    val b = mutable.Buffer[List[A]]()
    val used = mutable.Set[A]()
    _permutations2(Nil, used /*Set()*/, 0)

    def _permutations2(cur: List[A], used: mutable.Set[A], depth: Int): Unit = {
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if (!used.contains(e))
        } {
          _permutations2(e :: cur, used += e, depth + 1)
          // backtrack
          used.remove(e)
        }
      }
    }

    b.toList

  }

  def permutations[A](l: List[A], k: Int): List[List[A]] = {
    def combine(e: A, l: List[List[A]]) : List[List[A]] = {
      l.flatMap { ll =>
        (0 to ll.length).map { i =>
          val (pre, suf) = ll.splitAt(i)
          pre ++ List(e) ++ suf
        }
      }
    }

    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => permutations(t, k) ++ combine(h, permutations(t, k - 1))
        case _ => Nil
      }
    }
  }

  /** @return successive r length permutations of elements in the list. */
  def permutationsWithReplacement[A](l: List[A], k: Int): List[List[A]] = {
    if (k == 1) l.map(List(_)) else
      l.flatMap(e => permutationsWithReplacement(l, k-1).map(ll => e :: ll))
  }

  /** @return r length subsequences of elements from the input list. */
  def combinations[A](l: List[A], k: Int): List[List[A]] =
    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => combinations(t, k) ++ combinations(t, k - 1).map(l => h :: l)
        case _      => Nil
      }
    }

  /** @return r length subsequences of elements from the input list allowing individual elements to be repeated more than once. */
  def combinationsWithReplacement[A](l: List[A], k: Int): List[List[A]] =
    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => combinationsWithReplacement(t, k) ++ combinationsWithReplacement(l, k - 1).map(l => h :: l)
        case _ => Nil
      }
    }

  List(2) ::: List(1)

  println(permutationsWithReplacement(List("A", "B", "C", "D"), 2))
  println(permutationsWithReplacement(List("A", "B", "C", "D"), 2).size)

  println(permutations(List("A", "B", "C", "D"), 2))
  println(permutations(List("A", "B", "C", "D"), 2).size)

  println(combinationsWithReplacement(List("A", "B", "C", "D"), 2))
  println(combinationsWithReplacement(List("A", "B", "C", "D"), 2).size)

  println(combinations(List("A", "B", "C", "D"), 2))
  println(combinations(List("A", "B", "C", "D"), 2).size)
}
