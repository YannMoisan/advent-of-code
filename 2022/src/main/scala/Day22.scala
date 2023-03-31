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

  def next2(grid: Grid[Char], state: (Int, Direction4), s: String): (Int, Direction4) =
    s match {
      case "L" => (state._1, prev(state._2, Direction4.all))
      case "R" => (state._1, next(state._2, Direction4.all))
      case forward =>
        val length = forward.toInt
        val targetPos = Iterator
          .iterate(grid.dim.pos(state._1)) { pos =>
            val position    = Position(pos.x, pos.y)
            val newPosition = Position.move(position, state._2)
            val newPos      = Pos(newPosition.x, newPosition.y)

            // need to wrap ?
            if (newPos.x < 0 || newPos.x >= grid.dim.width || newPos.y < 0 || newPos.y >= grid.dim.height || grid(
                  newPos
                ) == ' ') {
              val candidate = state._2 match {
                case Direction4.Right =>
                  (0 until grid.dim.width).map(Pos(_, newPos.y)).find(grid(_) != ' ')
                case Direction4.Left =>
                  (0 until grid.dim.width).reverse.map(Pos(_, newPos.y)).find(grid(_) != ' ')
                case Direction4.Up =>
                  (0 until grid.dim.height).reverse.map(Pos(newPos.x, _)).find(grid(_) != ' ')
                case Direction4.Down =>
                  (0 until grid.dim.height).map(Pos(newPos.x, _)).find(grid(_) != ' ')
              }
              if (grid(candidate.get) == '#') pos else candidate.get
            } else if (grid(newPos) == '#') pos
            else newPos
          // is it a wall ?
          }.drop(length).next()
        (grid.dim.index(targetPos), state._2)
    }

  override def part2(input: Iterator[String]): Int = 42
}
