package tt

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

// https://www.codingame.com/ide/puzzle/bulls-and-cows-2
object Player extends App {
  val numberLength = readLine().toInt

  var lastGuess: String = _
  val guesses           = mutable.Buffer[Guess]()

  val candidates: Array[Byte] = Combinatorics
    .permutationsSB9flatNoLambda(numberLength)

  var from = 0

  System.gc()

  // game loop
  while (true) {
    val Array(bulls, cows) = (readLine() split " ").filter(_ != "").map(_.toInt)
    val s                  = System.nanoTime()
    if (bulls != -1) {
      guesses += Guess(lastGuess.map(c => Integer.valueOf(c.toString).toByte).toArray, bulls, cows)
    }

    lastGuess = if (guesses.isEmpty) {
      "1234567890".take(numberLength)
    } else {
      Console.err.println(s"from=$from/${candidates.size}")
      from = BruteForce.solve(guesses.toList, from, candidates)
      candidates.slice(from*numberLength, (from +1)*numberLength).mkString
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    val e = System.nanoTime()
    Console.err.println(s"${(e - s) / 1000000}ms")
    println(lastGuess) // number with numberLength count of digits
  }
}

case class Guess(guess: Array[Byte], bulls: Int, cows: Int) {
  val value = bulls << 4 | cows
}

trait BullsAndCows {
  def solve(guesses: List[Guess], from: Int, candidates: Array[Byte]): Int
}

object BruteForce extends BullsAndCows {
  override def solve(guesses: List[Guess], from: Int, candidates: Array[Byte]): Int = {
    val s = System.nanoTime()
    var e = System.nanoTime()
    var found = false
    var i = from
    while (!found && (e-s) < 50 * 1000 * 1000) {
      if (i % 10000 == 0) {
        e = System.nanoTime()
      }
      if (guesses.forall(guess => evaluate(candidates, i, guess.guess) == guess.value)) {
        found = true
      } else {
        i += 1
      }
    }
    i
  }


  private def evaluate(sol: Array[Byte], solIdx: Int, guess: Array[Byte]): Int = {
      // Find bulls
      var bulls = 0
      var cows = 0
      // populate the state for the algo
      var state = 0 // Array.ofDim[Boolean](10)
      //mutable.Map[Char, Int]()
      var i = 0
      val len = guess.length
      while (i < len) {
        val s = sol(solIdx * len + i)
        val g = guess(i)
        if (s == g) {
          bulls += 1
        } else {
          if ((state & 1 << s.toInt) > 0)
            cows += 1
          if ((state & 1 << g.toInt) > 0)
            cows += 1
          state |= 1 << s.toInt
          state |= 1 << g.toInt
        }
        i += 1
      }
      bulls << 4 | cows
    }

  }

object Combinatorics extends App {

  def permutationsSB9flatNoLambda(k: Int): Array[Byte] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(k).product
    var c = 0
    val b = Array.ofDim[Byte](count * k)
    val used = Array.ofDim[Boolean](10)
    val cur = Array.ofDim[Byte](k)
    val l0 = (1 to 9).map(_.toByte).toArray
    val ln = (0 to 9).map(_.toByte).toArray
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit = {
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, c * k, k)
        //cur.clone()
        c += 1
        //        }
      } else {
        val ll = if (depth == 0) l0 else ln
        var llCount = 0
        while (llCount < ll.length) {
          val e = ll(llCount)
          val idx = e // - '0'
          if (!used(idx.toInt)) {
            used(idx.toInt) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx.toInt) = false
          }
          llCount += 1
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
