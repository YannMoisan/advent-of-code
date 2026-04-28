import com.yannmoisan.util.collection.{next, prev}
import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.{Direction4, Grid, Grid1D, Pos}

import scala.util.matching.Regex

object Day22 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val list         = input.toList
    val grid         = Grid1D(list.dropRight(2).iterator.map(_.padTo(150, ' ')))
    val instructions = list.last

    // You begin the path in the leftmost open tile of the top row of tiles.
    // Initially, you are facing to the right (from the perspective of how the map is drawn).
    val startPos: (Int, Direction4) = (grid.dim.indices.find(grid(_) == '.').get, Direction4.Right)

    val it: Regex.MatchIterator = """\d+|L|R""".r.findAllIn(instructions)
    val end                     = it.foldLeft(startPos) { case (acc, inst) => next2(grid, acc, inst) }
    val endPos                  = grid.dim.pos(end._1)
    // Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
    (endPos.y + 1) * 1000 + (endPos.x + 1) * 4 + (end._2 match {
      case Direction4.Up    => 3
      case Direction4.Right => 0
      case Direction4.Down  => 1
      case Direction4.Left  => 2
    })
  }

  override def part2(input: Iterator[String]): Int = {
    val list         = input.toList
    val grid         = Grid1D(list.dropRight(2).iterator.map(_.padTo(150, ' ')))
    val instructions = list.last

    // You begin the path in the leftmost open tile of the top row of tiles.
    // Initially, you are facing to the right (from the perspective of how the map is drawn).
    val startPos: (Int, Direction4) = (grid.dim.indices.find(grid(_) == '.').get, Direction4.Right)

    val it: Regex.MatchIterator = """\d+|L|R""".r.findAllIn(instructions)
    val end                     = it.foldLeft(startPos) { case (acc, inst) => next3(grid, acc, inst) }

    val endPos = grid.dim.pos(end._1)
    // Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
    (endPos.y + 1) * 1000 + (endPos.x + 1) * 4 + (end._2 match {
      case Direction4.Up    => 3
      case Direction4.Right => 0
      case Direction4.Down  => 1
      case Direction4.Left  => 2
    })
  }

  def next2(grid: Grid[Char], state: (Int, Direction4), s: String): (Int, Direction4) =
    s match {
      case "L" => (state._1, prev(state._2, Direction4.all))
      case "R" => (state._1, next(state._2, Direction4.all))
      case forward =>
        val length = forward.toInt
        val targetPosAndDir = Iterator
          .iterate((grid.dim.pos(state._1), state._2)) { case (pos, _) =>
            val position    = Position(pos.x, pos.y)
            val newPosition = Position.move(position, state._2)
            val newPos      = Pos(newPosition.x, newPosition.y)

            // need to wrap ?
            if (
              newPos.x < 0 || newPos.x >= grid.dim.width || newPos.y < 0 || newPos.y >= grid.dim.height || grid(
                newPos
              ) == ' '
            ) {
              val candidate = wrap1(newPos, state._2, grid)
              if (grid(candidate._1) == '#') (pos, state._2) else candidate
            } else if (grid(newPos) == '#') (pos, state._2)
            else (newPos, state._2)
          // is it a wall ?
          }.drop(length).next()
        (grid.dim.index(targetPosAndDir._1), state._2)
    }

  def next3(grid: Grid[Char], state: (Int, Direction4), s: String): (Int, Direction4) =
    s match {
      case "L" => (state._1, prev(state._2, Direction4.all))
      case "R" => (state._1, next(state._2, Direction4.all))
      case forward =>
        val length = forward.toInt
        val targetPosAndDir = Iterator
          .iterate((grid.dim.pos(state._1), state._2)) { case (pos, dir) =>
            val position    = Position(pos.x, pos.y)
            val newPosition = Position.move(position, dir)
            val newPos      = Pos(newPosition.x, newPosition.y)

            // need to wrap ?
            if (
              newPos.x < 0 || newPos.x >= grid.dim.width || newPos.y < 0 || newPos.y >= grid.dim.height || grid(
                newPos
              ) == ' '
            ) {
              val candidate = wrap2(newPos)
              if (grid(candidate._1) == '#') (pos, dir) else candidate
            } else if (grid(newPos) == '#') (pos, dir)
            else (newPos, dir)
          // is it a wall ?
          }.drop(length).next()
        (grid.dim.index(targetPosAndDir._1), targetPosAndDir._2)
    }

  def wrap1(newPos: Pos, dir: Direction4, grid: Grid[Char]): (Pos, Direction4) =
    (
      dir match {
        case Direction4.Right =>
          (0 until grid.dim.width).map(Pos(_, newPos.y)).find(grid(_) != ' ').get
        case Direction4.Left =>
          (0 until grid.dim.width).reverse.map(Pos(_, newPos.y)).find(grid(_) != ' ').get
        case Direction4.Up =>
          (0 until grid.dim.height).reverse.map(Pos(newPos.x, _)).find(grid(_) != ' ').get
        case Direction4.Down =>
          (0 until grid.dim.height).map(Pos(newPos.x, _)).find(grid(_) != ' ').get
      },
      dir
    )

  // ...111222
  // ...111222
  // ...111222
  // ...333...
  // ...333...
  // ...333...
  // 444555...
  // 444555...
  // 444555...
  // 666......
  // 666......
  // 666......
  def wrap2(pos: Pos): (Pos, Direction4) =
    if (pos.y == -1 && pos.x >= 50 && pos.x < 100) // 1 to 6
      (Pos(0, 150 + pos.x - 50), Direction4.Right)
    else if (pos.x == -1 && pos.y >= 150 && pos.y < 200) // 6 to 1
      (Pos(50 + pos.y - 150, 0), Direction4.Down)
    else if (pos.y == -1 && pos.x >= 100 && pos.x < 150) // 2 to 6
      (Pos(0 + pos.x - 100, 199), Direction4.Up)
    else if (pos.y == 200 && pos.x >= 0 && pos.x < 50) // 6 to 2
      (Pos(100 + pos.x, 0), Direction4.Down)
    else if (pos.x == 49 && pos.y >= 0 && pos.y < 50) // 1 to 4
      (Pos(0, 100 + (49 - pos.y)), Direction4.Right)
    else if (pos.x == -1 && pos.y >= 100 && pos.y < 150)
      (Pos(50, 0 + (149 /*99*/ - pos.y)), Direction4.Right) // 4 to 1
    else if (pos.x == 150 && pos.y >= 0 && pos.y < 50)      // 2 to 5
      (Pos(99, 100 + (49 - pos.y)), Direction4.Left)
    else if (pos.x == 100 && pos.y >= 100 && pos.y < 150)
      (Pos(149, 0 + (149 - pos.y)), Direction4.Left)     // 5 to 2
    else if (pos.y == 50 && pos.x >= 100 && pos.x < 150) // 2 to 3
      (Pos(99, 50 + pos.x - 100), Direction4.Left)
    else if (pos.x == 100 && pos.y >= 50 && pos.y < 100) // 3 to 2
      (Pos(100 + pos.y - 50, 49), Direction4.Up)
    else if (pos.x == 49 && pos.y >= 50 && pos.y < 100) // 3 to 4
      (Pos(0 + pos.y - 50, 100), Direction4.Down)
    else if (pos.y == 99 && pos.x >= 0 && pos.x < 50) // 4 to 3
      (Pos(50, 50 + pos.x), Direction4.Right)
    else if (pos.y == 150 && pos.x >= 50 && pos.x < 100) // 5 to 6
      (Pos(49, 150 + pos.x - 50), Direction4.Left)
    else if (pos.x == 50 && pos.y >= 150 && pos.y < 200) // 6 to 5
      (Pos(50 + pos.y - 150, 149), Direction4.Up)
    else throw new IllegalStateException(pos.toString)
}
