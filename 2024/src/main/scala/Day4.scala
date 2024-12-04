import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day4 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)

    val horizontalWords = for {
      x <- 0 until grid.dim.width - 3
      y <- 0 until grid.dim.height
    } yield {
      (0 to 3).map(i => Pos(x + i, y)).map(grid(_)).mkString
    }

    val verticalWords = for {
      x <- 0 until grid.dim.width
      y <- 0 until grid.dim.height - 3
    } yield {
      (0 to 3).map(i => Pos(x, y + i)).map(grid(_)).mkString
    }

    val fstDiagonalWords = for {
      x <- 0 until grid.dim.width - 3
      y <- 0 until grid.dim.height - 3
    } yield {
      (0 to 3).map(i => Pos(x + i, y + i)).map(grid(_)).mkString
    }

    val sndDiagonalWords = for {
      x <- 0 until grid.dim.width - 3
      y <- 0 until grid.dim.height - 3
    } yield {
      (0 to 3).map(i => Pos(x + 3 - i, y + i)).map(grid(_)).mkString
    }

    val allWords = horizontalWords ++ verticalWords ++ fstDiagonalWords ++ sndDiagonalWords
    allWords.count(w => w == "XMAS" || w == "SAMX")
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)

    val words = for {
      x <- 0 until grid.dim.width - 2
      y <- 0 until grid.dim.height - 2
    } yield {
      Seq(
        Pos(x, y),
        Pos(x + 1, y + 1),
        Pos(x + 2, y + 2),
        Pos(x, y + 2),
        Pos(x + 2, y)
      ).map(grid(_)).mkString
    }
    words.count(w => List("MASMS", "MASSM", "SAMMS", "SAMSM").contains(w))

  }
}
