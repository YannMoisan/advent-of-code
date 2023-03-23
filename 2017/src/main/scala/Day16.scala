import com.yannmoisan.util.collection.firstDuplicateIndex

object Day16 extends SinglePuzzle[String, String] {
  sealed trait Move
  case class Spin(x: Int)              extends Move
  case class Exchange(a: Int, b: Int)  extends Move
  case class Partner(a: Char, b: Char) extends Move

  override def part1(input: String): String = {
    val init: Array[Char] = ('a' to 'p').toArray
    val moves: Array[Move] = input
      .split(",")
      .map {
        case s"s$x"    => Spin(x.toInt)
        case s"x$a/$b" => Exchange(a.toInt, b.toInt)
        case s"p$a/$b" => Partner(a.head, b.head)
      }
    moves.foldLeft(init)(apply).mkString
  }

  def apply(programs: Array[Char], move: Move): Array[Char] =
    move match {
      case Spin(x) =>
        programs.slice(programs.length - x, programs.length) ++ programs.slice(
          0,
          programs.length - x
        )
      case Exchange(a, b) =>
        val n = programs.clone()
        n(a) = programs(b)
        n(b) = programs(a)
        n
      case Partner(a, b) =>
        val n = programs.clone()
        n(programs.indexOf(a)) = programs(programs.indexOf(b))
        n(programs.indexOf(b)) = programs(programs.indexOf(a))
        n
    }

  override def part2(input: String): String = {
    val init: Array[Char] = ('a' to 'p').toArray
    val moves: Array[Move] = input
      .split(",")
      .map {
        case s"s$x"    => Spin(x.toInt)
        case s"x$a/$b" => Exchange(a.toInt, b.toInt)
        case s"p$a/$b" => Partner(a.head, b.head)
      }
    moves.foldLeft(init)(apply).mkString

    // Find a cycle
    val it  = Iterator.iterate(init)(arr => moves.foldLeft(arr)(apply)).map(_.mkString)
    val mod = firstDuplicateIndex(it).get

    val repetition = (1000000000L % mod).toInt
    Iterator.iterate(init)(arr => moves.foldLeft(arr)(apply)).drop(repetition).next().mkString
  }
}
