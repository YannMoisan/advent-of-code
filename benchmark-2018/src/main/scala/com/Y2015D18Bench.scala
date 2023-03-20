package com

import com.yannmoisan.util.grid.Grid1D
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@Threads(1)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
class Y2015D18Bench {
  var grid: Array[Array[Char]]       = _
  var flatgrid: Array[Char]          = _
  var flatgridbool: Array[Boolean]   = _
  var flatgridbool2: Grid1D[Boolean] = _
  val offsets = List(
    (-1, -1),
    (-1, 0),
    (-1, +1),
    (-0, -1),
    (-0, +1),
    (+1, -1),
    (+1, 0),
    (+1, +1)
  )

  //@Benchmark
  def a_baseline(): Unit = {
    val end = (1 to 100).foldLeft(grid) { case (acc, _) => nextState(acc, neighborIndices) }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def b_flatten(): Unit = {
    val end = (1 to 100).foldLeft(grid) { case (acc, _) => nextState(acc, neighborIndices_flatten) }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def c_flatten_better(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState(acc, neighborIndices_flatten_better)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def d_offset(): Unit = {
    val end = (1 to 100).foldLeft(grid) { case (acc, _) => nextState(acc, neighborIndices_offset) }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def e_offset_constants(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState(acc, neighborIndices_offset_constants)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def f_offset_better_for(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState(acc, neighborIndices_offset_better_for)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def g_offset_collect(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState(acc, neighborIndices_offset_collect)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def h_offset_pool(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState(acc, neighborIndices_offset_pool)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def i_cache(): Unit = {
    val end = (1 to 100).foldLeft(grid) { case (acc, _) => nextState(acc, neighborIndices_cache) }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def j_foreach(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_foreach(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def k_while(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_while(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def l_no_lambda_call(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_no_lambda_call(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def m_no_method_call(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_no_method_call(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def n_vector_array(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_vector_array(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def o_inline3_colbased(): Unit = {
    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) => nextState_inline3_colbased(acc, neighborIndices_cache)
    }
    assert(end.map(_.count(_ == '#')).sum == 814)
  }

  //@Benchmark
  def p_1d_foreach(): Unit = {
    val end = (1 to 100).foldLeft(flatgrid) { case (acc, _) => nextState_1d_foreach(acc) }
    assert(end.count(_ == '#') == 814)
  }

  //@Benchmark
  def q_1d_while(): Unit = {
    val end = (1 to 100).foldLeft(flatgrid) { case (acc, _) => nextState_1d_while(acc) }
    assert(end.count(_ == '#') == 814)
  }

  @Benchmark
  def s_1d_bool(): Unit = {
    val end = (1 to 100).foldLeft(flatgridbool) { case (acc, _) => nextState_1d_bool(acc) }
    assert(end.count(_ == true) == 814)
  }

  @Benchmark
  def s_1d_bool_grid(): Unit = {
    val end = (1 to 100).foldLeft(flatgridbool2) { case (acc, _) => nextState_1d_bool_grid(acc) }
    assert(end.dim.indices.toIndexedSeq.count(p => end(p)) == 814)
  }

  def nextState(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndices(row, col)
      val neighborsOn = neighborIndices_.count {
        case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#'
      }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_foreach(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndices(row, col)
      var neighborsOn      = 0
      neighborIndices_.foreach {
        case (neighborRow, neighborCol) =>
          if (grid(neighborRow)(neighborCol) == '#') neighborsOn += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_while(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndices(row, col)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index._1)(index._2) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_no_lambda_call(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndices_cache(row, col)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index._1)(index._2) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_no_method_call(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndicesCache(row)(col)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index._1)(index._2) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_vector_array(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      val neighborIndices_ = neighborIndicesCacheArray(row)(col)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index._1)(index._2) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def nextState_1d_foreach(grid: Array[Char]): Array[Char] = {
    val newState = Array.ofDim[Char](10000)

    (0 until 10000)
      .foreach { index =>
        val neighborIndices_ = neighborIndicesCacheArray1d(index)
        var neighborsOn      = 0
        var i                = 0
        while (i < neighborIndices_.length) {
          val index = neighborIndices_(i)
          if (grid(index) == '#') neighborsOn += 1
          i += 1
        }
        //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
        newState(index) = (grid(index), neighborsOn) match {
          case ('#', 2) | ('#', 3) => '#'
          case ('#', _)            => '.'
          case ('.', 3)            => '#'
          case ('.', _)            => '.'
        }
      }
    newState
  }

  def nextState_1d_while(grid: Array[Char]): Array[Char] = {
    val newState = Array.ofDim[Char](10000)

    var index = 0
    while (index < 10000) {
      val neighborIndices_ = neighborIndicesCacheArray1d(index)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(index) = (grid(index), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
      index += 1
    }
    newState
  }

  def nextState_1d_bool(grid: Array[Boolean]): Array[Boolean] = {
    val newState = Array.ofDim[Boolean](10000)

    var index = 0
    while (index < 10000) {
      val neighborIndices_ = neighborIndicesCacheArray1d(index)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index)) neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(index) = (grid(index), neighborsOn) match {
        case (true, 2) | (true, 3) => true
        case (true, _)             => false
        case (false, 3)            => true
        case (false, _)            => false
      }
      index += 1
    }
    newState
  }

  def nextState_1d_bool_grid(grid: Grid1D[Boolean]): Grid1D[Boolean] = {
    val newState = new Grid1D(Array.ofDim[Boolean](10000), 100, 100)

    var index = 0
    while (index < 10000) {
      val neighbors   = grid.dim.neighbors8(index)
      var neighborsOn = 0
      var i           = 0
      while (i < neighbors.length) {
        val index = neighbors(i)
        if (grid(index)) neighborsOn += 1
        i += 1
      }
      newState(index) = (grid(index), neighborsOn) match {
        case (true, 2) | (true, 3) => true
        case (true, _)             => false
        case (false, 3)            => true
        case (false, _)            => false
      }
      index += 1
    }
    newState
  }

  def nextState_inline3_colbased(
      grid: Array[Array[Char]],
      neighborIndices: (Int, Int) => Seq[(Int, Int)]
  ): Array[Array[Char]] = {
    val newState = Array.ofDim[Char](grid.size, grid.head.size)

    for {
      col <- grid.head.indices
      row <- grid.indices
    } {
      val neighborIndices_ = neighborIndicesCache(row)(col)
      var neighborsOn      = 0
      var i                = 0
      while (i < neighborIndices_.length) {
        val index = neighborIndices_(i)
        if (grid(index._1)(index._2) == '#') neighborsOn += 1
        i += 1
      }
      //{ case (neighborRow, neighborCol) => grid(neighborRow)(neighborCol) == '#' }
      newState(row)(col) = (grid(row)(col), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
    newState
  }

  def neighborIndices(row: Int, col: Int): Seq[(Int, Int)] =
    for {
      nrow <- row - 1 to row + 1
      ncol <- col - 1 to col + 1
      if nrow >= 0 && nrow < 100 && ncol >= 0 && ncol < 100
      if nrow != row || ncol != col
    } yield (nrow, ncol)

  def neighborIndices_flatten(row: Int, col: Int): Seq[(Int, Int)] =
    for {
      indices <- Seq(
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row - 0, col - 1),
        (row - 0, col),
        (row - 0, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1)
      )
      if indices._1 >= 0 && indices._1 < 100 && indices._2 >= 0 && indices._2 < 100
      if indices._1 != row || indices._2 != col
    } yield indices

  def neighborIndices_flatten_better(row: Int, col: Int): Seq[(Int, Int)] =
    for {
      indices <- Seq(
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row - 0, col - 1),
        (row - 0, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1)
      )
      if indices._1 >= 0 && indices._1 < 100 && indices._2 >= 0 && indices._2 < 100
    } yield indices

  def neighborIndices_offset(row: Int, col: Int): Seq[(Int, Int)] =
    for {
      offset <- Seq(
        (-1, -1),
        (-1, 0),
        (-1, +1),
        (-0, -1),
        (-0, +1),
        (+1, -1),
        (+1, 0),
        (+1, +1)
      )
      index = (row + offset._1, col + offset._2)
      if index._1 >= 0 && index._1 < 100 && index._2 >= 0 && index._2 < 100
    } yield index

  def neighborIndices_offset_constants(row: Int, col: Int): Seq[(Int, Int)] =
    for {
      offset <- offsets
      index = (row + offset._1, col + offset._2)
      if index._1 >= 0 && index._1 < 100 && index._2 >= 0 && index._2 < 100
    } yield index

  def neighborIndices_offset_better_for(row: Int, col: Int): Seq[(Int, Int)] =
    offsets
      .map(offset => (row + offset._1, col + offset._2))
      .filter(index => index._1 >= 0 && index._1 < 100 && index._2 >= 0 && index._2 < 100)

  def neighborIndices_offset_collect(row: Int, col: Int): Seq[(Int, Int)] =
    offsets
      .collect {
        case (or, oc) if (row + or >= 0 && row + or < 100 && col + oc >= 0 && col + oc < 100) =>
          (row + or, col + oc)
      }

  def neighborIndices_offset_pool(row: Int, col: Int): Seq[(Int, Int)] =
    offsets
      .collect {
        case (or, oc) if (row + or >= 0 && row + or < 100 && col + oc >= 0 && col + oc < 100) =>
          indicesPool(row + or)(col + oc)
      }

  def neighborIndices_cache(row: Int, col: Int): Seq[(Int, Int)] =
    neighborIndicesCache(row)(col)

  val indicesPool = {
    val arr = Array.ofDim[(Int, Int)](100, 100)
    for {
      r <- 0 until 100
      c <- 0 until 100
    } {
      arr(r)(c) = (r, c)
    }
    arr
  }

  val neighborIndicesCache: Array[Array[Seq[(Int, Int)]]] = {
    val arr = Array.ofDim[Seq[(Int, Int)]](100, 100)
    for {
      r <- 0 until 100
      c <- 0 until 100
    } {
      arr(r)(c) = neighborIndices(r, c)
    }
    arr
  }

  val neighborIndicesCacheArray: Array[Array[Array[(Int, Int)]]] = {
    val arr = Array.ofDim[Array[(Int, Int)]](100, 100)
    for {
      r <- 0 until 100
      c <- 0 until 100
    } {
      arr(r)(c) = neighborIndices(r, c).toArray
    }
    arr
  }

  val neighborIndicesCacheArray1d: Array[Array[Int]] = {
    val arr = Array.ofDim[Array[Int]](10000)
    for {
      r <- 0 until 100
      c <- 0 until 100
    } {
      arr(r * 100 + c) = neighborIndices(r, c).map { case (r, c) => r * 100 + c }.toArray
    }
    arr
  }

  @Setup
  def setup(): Unit = {
    val is = this.getClass.getClassLoader.getResourceAsStream("input18")
    grid = io.Source.fromInputStream(is).getLines().toArray.map(_.toArray)
    flatgrid = grid.flatten
    flatgridbool = flatgrid.map(_ == '#')
    flatgridbool2 = new Grid1D(flatgrid.map(_ == '#'), 100, 100)
  }
}
