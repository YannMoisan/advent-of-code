package com

import com.yannmoisan.util.grid.{Grid1D, Pos}
import fp.Functional
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, Setup, State, Threads, Warmup}

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
class Y2021D25Bench {
  var grid: Grid1D[Char] = _

  @Setup
  def setup(): Unit = {
    val is = this.getClass.getClassLoader.getResourceAsStream("input18")
    val tmp: Array[Array[Char]] = io.Source.fromInputStream(is).getLines().toArray.map(_.toArray)
    grid = Grid1D(tmp)
  }

  @Benchmark
  def test() = {
    new Functional[Grid1D[Char]]().convergesIn(next, grid, _.equals(_)) + 1

  }

  def `next>`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.allPos.foreach { p =>
      val pn = Pos((p.x + 1) % g.dim.width, p.y)(g.dim)
      if (g(p) == '>' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = '>'
      }
    }
    n
  }

  def `nextv`(g: Grid1D[Char]): Grid1D[Char] = {
    val n = g.copy()
    g.dim.allPos.foreach { p =>
      val pn = Pos(p.x, (p.y + 1) % g.dim.height)(g.dim)
      if (g(p) == 'v' && g(pn) == '.') {
        n(p) = '.'
        n(pn) = 'v'
      }
    }
    n
  }

  def next(g: Grid1D[Char]): Grid1D[Char] = nextv(`next>`(g))

}

