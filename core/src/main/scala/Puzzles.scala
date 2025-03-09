import scala.io.Codec.defaultCharsetCodec
import scala.reflect.runtime.{universe => ru}
import scala.util.Try

trait Puzzle[O1, O2] {
  type I

  def input: I

  def part1(input: I): O1
  def part2(input: I): O2

  def run(): Unit = {
    val p1 = Timer.withTimer(part1(input))
    println(s"[day=${day()}] (t=${p1._2.toString}ms) ${p1._1.toString}")

    val p2 = Timer.withTimer(part2(input))
    println(s"[day=${day()}] (t=${p2._2.toString}ms) ${p2._1.toString}")
  }

  protected def lines = {
    val is = this.getClass.getClassLoader.getResourceAsStream(filename)
    io.Source.fromInputStream(is)(defaultCharsetCodec).getLines()
  }

  def day() = {
    val clazz = this.getClass.getSimpleName
    clazz.substring(3, clazz.length - 1)
  }

  private def filename =
    s"input${day()}"
}

trait MultiPuzzle[O1, O2] extends Puzzle[O1, O2] {
  type I = Iterator[String]
  override def input = lines
}

trait SinglePuzzle[O1, O2] extends Puzzle[O1, O2] {
  type I = String
  override def input = lines.next()
}

object Puzzles {
  def main(args: Array[String]) = {
    val puzzles = findPuzzles()

    val day = if (args.length == 1) {
      Some(args(0))
    } else {
      None
    }

    day match {
      case Some(d) =>
        puzzles.find(_.day() == d) match {
          case Some(puzzle) => puzzle.run()
          case None         => sys.error(s"Unknown day '$d'")
        }
      case None => puzzles.foreach(_.run())
    }
  }

  // use runtime reflection to detect implemented puzzles
  def findPuzzles(): Seq[Puzzle[_, _]] = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    (1 to 25).flatMap { day =>
      Try {
        val ccr = m.staticModule(s"Day$day")
        m.reflectModule(ccr).instance.asInstanceOf[Puzzle[_, _]]
      }.toOption
    }
  }
}

object Timer {
  def withTimer[A](a: => A): (A, Long) = {
    val s   = System.nanoTime()
    val ret = a
    val e   = System.nanoTime()
    (ret, (e - s) / (1000 * 1000))
  }
}
