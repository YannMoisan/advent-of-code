package com

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, State, Threads, Warmup}

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
class BruteForceBench {

  var numberLength = 7

  @Benchmark
  def test(): Int =
    BruteForce.solve(List(Guess("1234567", 3 << 4)), 0)
}

case class Guess(guess: String, v: Int)

trait BullsAndCows {
  def solve(guesses: List[Guess], from: Int): Int
}

object BruteForce extends BullsAndCows {
  val candidates = Combinatorics
    .permutationsSB8(('0' to '9').toList, 7)

  override def solve(guesses: List[Guess], from: Int): Int =
    candidates.indexWhere(
      candidate => guesses.forall(guess => evaluate(candidate, guess.guess) == guess.v),
      from
    )

  private def evaluate(sol: Array[Char], guess: String): Int = {
    // Find bulls
    var bulls = 0
    var cows  = 0
    // populate the state for the algo

    //val state = Array.ofDim[Boolean](10)
    var state = 0

    //mutable.Map[Char, Int]()
    var i   = 0
    val len = guess.length
    while (i < len) {
      val s = sol(i)
      val g = guess.charAt(i)
      if (s == g) {
        bulls += 1
      } else {
        //if (state(s - '0'))
        if ((state & (1 << (s - '0'))) > 0)
          cows += 1
        if ((state & (1 << (g - '0'))) > 0) {
          //        if (state(g - '0'))
          cows += 1
        }
        state |= 1 << s - '0'
        state |= 1 << g - '0'
        //        state(s - '0') = true
        //        state(g - '0') = true
      }
      i += 1
    }
    bulls << 4 | cows
  }

}

object Combinatorics extends App {

  def permutationsSB8(l: List[Char], k: Int): ArrayBuffer[Array[Char]] = {
    // dfs
    val count = (10 until 10 - k by -1).product
    val b     = new mutable.ArrayBuffer[Array[Char]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](k)
    _permutationsSB8(0)

    def _permutationsSB8(depth: Int): Unit =
      if (depth == k) {
        if (cur.head != '0') {
          b += cur.clone()
        }
      } else {
        l.foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB8(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }
    b
  }

  def permutationsSB7(l: List[Char], k: Int): ArrayBuffer[String] = {
    // dfs
    val count = (10 until 10 - k by -1).product
    val b     = new mutable.ArrayBuffer[String](count)
    val used  = Array.ofDim[Boolean](10)
    _permutationsSB7(new StringBuilder(k), 0)

    def _permutationsSB7(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        if (cur.head != '0') {
          b += cur.result()
        }
      } else {
        l.foreach { e =>
          val idx = e - '0'
          if (!used(e - '0')) {
            used(idx) = true
            _permutationsSB7(cur.append(e), depth + 1)
            // backtrack
            cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutations3(l: List[Char], k: Int): List[List[Char]] = {
    // dfs
    val b    = mutable.Buffer[List[Char]]()
    val used = Array.ofDim[Boolean](10)
    _permutations3(Nil, 0)

    def _permutations3(cur: List[Char], depth: Int): Unit =
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutations3(e :: cur, depth + 1)
          // backtrack
          used(idx) = false
        }
      }

    b.toList

  }

  def permutations4(l: List[Char], k: Int): List[String] = {
    // dfs
    val b    = mutable.Buffer[String]()
    val used = Array.ofDim[Boolean](10)
    _permutations4("", 0)

    def _permutations4(cur: String, depth: Int): Unit =
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutations4(cur + e, depth + 1)
          // backtrack
          used(idx) = false
        }
      }

    b.toList

  }

  def permutations2[A](l: List[A], k: Int): List[List[A]] = {
    // dfs
    val b    = mutable.Buffer[List[A]]()
    val used = mutable.Set[A]()
    _permutations2(Nil, used /*Set()*/, 0)

    def _permutations2(cur: List[A], used: mutable.Set[A], depth: Int): Unit =
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if !used.contains(e)
        } {
          _permutations2(e :: cur, used += e, depth + 1)
          // backtrack
          used.remove(e)
        }
      }

    b.toList

  }

  def permutations[A](l: List[A], k: Int): List[List[A]] = {
    def combine(e: A, l: List[List[A]]): List[List[A]] =
      l.flatMap { ll =>
        (0 to ll.length).map { i =>
          val (pre, suf) = ll.splitAt(i)
          pre ++ List(e) ++ suf
        }
      }

    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => permutations(t, k) ++ combine(h, permutations(t, k - 1))
        case _      => Nil
      }
    }
  }

  /** @return successive r length permutations of elements in the list. */
  def permutationsWithReplacement[A](l: List[A], k: Int): List[List[A]] =
    if (k == 1) l.map(List(_))
    else
      l.flatMap(e => permutationsWithReplacement(l, k - 1).map(ll => e :: ll))

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
        case h :: t =>
          combinationsWithReplacement(t, k) ++ combinationsWithReplacement(l, k - 1).map(l =>
            h :: l
          )
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
