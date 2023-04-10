package com

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, Setup, State, Threads, Warmup}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
class Y2015D10Bench {
  var input: String = _

  @Setup
  def setup(): Unit =
    input = List.fill(100)(Random.nextInt(3)).mkString

  @Benchmark
  def reference(): String =
    lookAndSay(input)

  @Benchmark
  def nobufferinitsizediter(): String =
    lookAndSayNoBufferInitSizedIter(input)

  @Benchmark
  def arraybuffer(): ArrayBuffer[Char] =
    lookAndSayArrayBuffer(input)
  //  lookAndSayNoBufferInit

  def lookAndSay(s: String): String = {
    val buf = new ArrayBuffer[(Char, Int)](s.length)
    (0 until s.length).foreach { i =>
      if (i == 0) {
        buf.addOne((s(0), 1))
      } else {
        val Some((lastCh, lastCount)) = buf.lastOption
        if (lastCh == s(i)) {
          val _ = buf.remove(buf.length - 1)
          buf.addOne((lastCh, lastCount + 1))
        } else
          buf.addOne((s(i), 1))
      }
    }
    val arr = Array.ofDim[Char](buf.length * 2)
    buf.indices.foreach { i =>
      arr(i * 2) = (buf(i)._2 + 48).toChar
      arr(i * 2 + 1) = buf(i)._1
    }
    new String(arr)
  }

  def lookAndSayNoBufferInitSizedIter(s: String): String = {
    val sb           = new StringBuilder()
    var lastCh: Char = s(0)
    var count: Int   = 1
    s.substring(1).foreach { ch =>
      if (lastCh == ch) {
        count += 1
      } else {
        sb.append((count + 48).toChar)
        sb.append(lastCh)
        lastCh = ch
        count = 1
      }
    }
    sb.append((count + 48).toChar)
    sb.append(lastCh)

    sb.toString()
  }

  def lookAndSayArrayBuffer(s: String): ArrayBuffer[Char] = {
    val sb           = new ArrayBuffer[Char]()
    var lastCh: Char = s(0)
    var count: Int   = 1
    s.substring(1).foreach { ch =>
      if (lastCh == ch) {
        count += 1
      } else {
        sb.append((count + 48).toChar)
        sb.append(lastCh)
        lastCh = ch
        count = 1
      }
    }
    sb.append((count + 48).toChar)
    sb.append(lastCh)

    sb
  }

}
