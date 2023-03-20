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
class BullsAndCowsGenerateBench {
  var numberLength = 10

  //@Benchmark
  def perm2(): List[String] =
    permutations2(('0' to '9').toList, numberLength)
      .map(xs => new String(xs.toArray))
      .filter(_.head != '0')

  //@Benchmark
  def perm3(): List[String] =
    permutations3(('0' to '9').toList, numberLength)
      .map(xs => new String(xs.toArray))
      .filter(_.head != '0')

  //@Benchmark
  def perm4(): List[String] =
    permutations3(('0' to '9').toList, numberLength)
      .map(xs => xs.mkString)
      .filter(_.head != '0')

  //@Benchmark
  def permSB(): List[String] =
    permutationsSB(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB2(): List[String] =
    permutationsSB2(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB3(): List[String] =
    permutationsSB3(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB4(): List[String] =
    permutationsSB4(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB5(): List[String] =
    permutationsSB5(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB6(): ArrayBuffer[String] =
    permutationsSB6(('0' to '9').toList, numberLength)
      .filter(_.head != '0')

  //@Benchmark
  def permSB7(): ArrayBuffer[String] =
    permutationsSB7(('0' to '9').toList, numberLength)

  //@Benchmark
  def permSB8(): ArrayBuffer[Array[Char]] =
    permutationsSB8(('0' to '9').toList, numberLength)

  //@Benchmark
  def permSB9(): ArrayBuffer[Array[Char]] =
    permutationsSB9(('0' to '9').toList, numberLength)

  //@Benchmark
  def permSB9bis(): ArrayBuffer[Array[Char]] =
    permutationsSB9bis(('0' to '9').toList, numberLength)

  //@Benchmark
  def permSB9ter(): Array[Array[Char]] =
    permutationsSB9ter(('0' to '9').toList, numberLength)

  //@Benchmark
  def permSB10(): ArrayBuffer[Array[Byte]] =
    permutationsSB10((0 to 9).map(_.toByte).toList, numberLength)

  @Benchmark
  def permSB9flat(): Array[Char] =
    permutationsSB9flat(('0' to '9').toList, numberLength)

  @Benchmark
  def permSB9flatNoLambda(): Array[Char] =
    permutationsSB9flatNoLambda(numberLength)

  //@Benchmark
  def permSB9flatByte(): Array[Byte] =
    permutationsSB9flatByte((0 to 9).map(_.toByte).toList, numberLength)

  //@Benchmark
  def permSB9flatByte2(): Array[Byte] =
    permutationsSB9flatByte2((0 to 9).map(_.toByte).toList, numberLength)

  //@Benchmark
  def permSB9flatByte3(): Array[Byte] =
    permutationsSB9flatByte3((0 to 9).map(_.toByte).toList, numberLength)

  //@Benchmark
  def permSB9flatByte4(): Array[Byte] =
    permutationsSB9flatByte4(numberLength)

  def permutations2[A](l: List[A], k: Int): List[List[A]] = {
    // dfs
    val b = mutable.Buffer[List[A]]()
    _permutations2(Nil, Set(), 0)

    def _permutations2(cur: List[A], used: Set[A], depth: Int): Unit =
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if !used.contains(e)
        } {
          _permutations2(e :: cur, used + e, depth + 1)
          // backtrack
        }
      }

    b.toList

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

  def permutationsSB(l: List[Char], k: Int): List[String] = {
    // dfs
    val b    = mutable.Buffer[String]()
    val used = Array.ofDim[Boolean](10)
    _permutationsSB(new StringBuilder(), 0)

    def _permutationsSB(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutationsSB(cur.append(e), depth + 1)
          // backtrack
          cur.deleteCharAt(cur.length() - 1)
          used(idx) = false
        }
      }

    b.toList

  }

  def permutationsSB2(l: List[Char], k: Int): List[String] = {
    // dfs
    val b    = mutable.Buffer[String]()
    val used = Array.ofDim[Boolean](10)
    _permutationsSB2(new StringBuilder(numberLength), 0)

    def _permutationsSB2(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutationsSB2(cur.append(e), depth + 1)
          // backtrack
          cur.deleteCharAt(cur.length() - 1)
          used(idx) = false
        }
      }

    b.toList

  }

  def permutationsSB3(l: List[Char], k: Int): List[String] = {
    // dfs
    val b    = mutable.Buffer[String]()
    val used = Array.ofDim[Boolean](10)
    _permutationsSB3(new StringBuilder(numberLength), 0)

    def _permutationsSB3(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutationsSB3(cur.append(e), depth + 1)
          // backtrack
          cur.setLength(depth)
          used(idx) = false
        }
      }

    b.toList

  }

  def permutationsSB4(l: List[Char], k: Int): List[String] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[String](count)
    val used  = Array.ofDim[Boolean](10)
    _permutationsSB4(new StringBuilder(numberLength), 0)

    def _permutationsSB4(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        for {
          e <- l
          if !used(e - '0')
        } {
          val idx = e - '0'
          used(idx) = true
          _permutationsSB4(cur.append(e), depth + 1)
          // backtrack
          cur.setLength(depth)
          used(idx) = false
        }
      }

    b.toList
  }

  def permutationsSB5(l: List[Char], k: Int): List[String] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[String](count)
    val used  = Array.ofDim[Boolean](10)
    _permutationsSB5(new StringBuilder(numberLength), 0)

    def _permutationsSB5(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        l.foreach { e =>
          val idx = e - '0'
          if (!used(e - '0')) {
            used(idx) = true
            _permutationsSB5(cur.append(e), depth + 1)
            // backtrack
            cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b.toList
  }

  def permutationsSB6(l: List[Char], k: Int): ArrayBuffer[String] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[String](count)
    val used  = Array.ofDim[Boolean](10)
    _permutationsSB6(new StringBuilder(numberLength), 0)

    def _permutationsSB6(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        b += cur.result()
      } else {
        l.foreach { e =>
          val idx = e - '0'
          if (!used(e - '0')) {
            used(idx) = true
            _permutationsSB6(cur.append(e), depth + 1)
            // backtrack
            cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB7(l: List[Char], k: Int): ArrayBuffer[String] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[String](count)
    val used  = Array.ofDim[Boolean](10)
    _permutationsSB7(new StringBuilder(numberLength), 0)

    def _permutationsSB7(cur: StringBuilder, depth: Int): Unit =
      if (depth == k) {
        if (cur.head != '0') {
          b += cur.result()
        }
      } else {
        l.foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
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

  def permutationsSB8(l: List[Char], k: Int): ArrayBuffer[Array[Char]] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[Array[Char]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
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

  def permutationsSB9(l: List[Char], k: Int): ArrayBuffer[Array[Char]] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[Array[Char]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
//        if (cur.head != '0') {
        b += cur.clone()
//        }
      } else {
        (if (depth == 0) '1' to '9' else l).foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }
    b
  }

  def permutationsSB9bis(l: List[Char], k: Int): ArrayBuffer[Array[Char]] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    val b     = new mutable.ArrayBuffer[Array[Char]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        b += cur.clone()
        //        }
      } else {
        (if (depth == 0) '1' to '9' else l).foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB9ter(l: List[Char], k: Int): Array[Array[Char]] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    var c     = 0
    val b     = Array.ofDim[Array[Char]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        b(c) = cur.clone()
        c += 1
        //        }
      } else {
        (if (depth == 0) '1' to '9' else l).foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB9flat(l: List[Char], k: Int): Array[Char] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    var c     = 0
    val b     = Array.ofDim[Char](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, c * numberLength, numberLength)
        //cur.clone()
        c += 1
        //        }
      } else {
        (if (depth == 0) '1' to '9' else l).foreach { e =>
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB9flatNoLambda(k: Int): Array[Char] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    var c     = 0
    val b     = Array.ofDim[Char](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Char](numberLength)
    val l0    = ('1' to '9').toArray
    val ln    = ('0' to '9').toArray
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, c * numberLength, numberLength)
        //cur.clone()
        c += 1
        //        }
      } else {
        val ll      = if (depth == 0) l0 else ln
        var llCount = 0
        while (llCount < ll.length) {
          val e   = ll(llCount)
          val idx = e - '0'
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
          llCount += 1
        }
      }

    b
  }

  def permutationsSB9flatByte(l: List[Byte], k: Int): Array[Byte] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    var c     = 0
    val b     = Array.ofDim[Byte](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Byte](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, c * numberLength, numberLength)
        //cur.clone()
        c += 1
        //        }
      } else {
        (if (depth == 0) (1 to 9).map(_.toByte) else l).foreach { e =>
          val idx = e.toInt
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB9flatByte2(l: List[Byte], k: Int): Array[Byte] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    var c     = 0
    val b     = Array.ofDim[Byte](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Byte](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, c * numberLength, numberLength)
        //cur.clone()
        c += 1
        //        }
      } else {
        (if (depth == 0) (1 to 9).map(_.toByte) else l).foreach { e =>
          val idx = e
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  class WrappedInt(var v: Int) {
    def inc(): Unit = v += 1
    def get(): Int  = v
  }
  def permutationsSB9flatByte3(l: List[Byte], k: Int): Array[Byte] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    val wi    = new WrappedInt(0)
    val b     = Array.ofDim[Byte](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Byte](numberLength)
    _permutationsSB9(0)

    def _permutationsSB9(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        System.arraycopy(cur, 0, b, wi.get() * numberLength, numberLength)
        //cur.clone()
        wi.inc()
        //        }
      } else {
        (if (depth == 0) (1 to 9).map(_.toByte) else l).foreach { e =>
          val idx = e
          if (!used(idx)) {
            used(idx) = true
            cur(depth) = e
            _permutationsSB9(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(idx) = false
          }
        }
      }

    b
  }

  def permutationsSB9flatByte4(k: Int): Array[Byte] = {
    // dfs
    val count = List(9, 9, 8, 7, 6, 5, 4, 3, 2, 1).take(numberLength).product
    val wi    = new WrappedInt(0)
    val b     = Array.ofDim[Byte](count * numberLength)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Byte](numberLength)
    val l0    = (1 to 9).map(_.toByte).toArray
    val ln    = (0 to 9).map(_.toByte).toArray
    _permutationsSB94(k, cur, b, wi, used, l0, ln, 0)

    b
  }

  def _permutationsSB94(
      k: Int,
      cur: Array[Byte],
      b: Array[Byte],
      wi: WrappedInt,
      used: Array[Boolean],
      l0: Array[Byte],
      ln: Array[Byte],
      depth: Int
  ): Unit =
    if (depth == k) {
      //        if (cur.head != '0') {
      System.arraycopy(cur, 0, b, wi.get() * numberLength, numberLength)
      //cur.clone()
      wi.inc()
      //        }
    } else {
      val ll = if (depth == 0) l0 else ln
      var a  = 0
      while (a < ll.length) {
        val idx = ll(a)
        if (!used(idx)) {
          used(idx) = true
          cur(depth) = idx
          _permutationsSB94(k, cur, b, wi, used, l0, ln, depth + 1)
          // backtrack
          //cur.setLength(depth)
          used(idx) = false
        }
        a += 1
      }
    }

  def permutationsSB10(l: List[Byte], k: Int): ArrayBuffer[Array[Byte]] = {
    // dfs
    val count = (10 until 10 - numberLength by -1).product
    val b     = new mutable.ArrayBuffer[Array[Byte]](count)
    val used  = Array.ofDim[Boolean](10)
    val cur   = Array.ofDim[Byte](numberLength)
    _permutationsSB10(0)

    def _permutationsSB10(depth: Int): Unit =
      if (depth == k) {
        //        if (cur.head != '0') {
        b += cur.clone()
        //        }
      } else {
        (if (depth == 0) (1 to 9).map(_.toByte) else l).foreach { e =>
          //val idx = e - '0'
          if (!used(e)) {
            used(e) = true
            cur(depth) = e
            _permutationsSB10(depth + 1)
            // backtrack
            //cur.setLength(depth)
            used(e) = false
          }
        }
      }

    b
  }

}
