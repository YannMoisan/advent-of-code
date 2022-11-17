import scala.annotation.tailrec

object Day5 extends MultiPuzzle[Int, Int] {

  final case class State(instr: IndexedSeq[Int], pos: Int, count: Int)

  def next(inc: Int => Int): State => State = { s =>
    State(
      s.instr.updated(s.pos, s.instr(s.pos) + inc(s.instr(s.pos))),
      s.pos + s.instr(s.pos),
      s.count + 1
    )
  }

  def stop: State => Boolean = s => s.pos < 0 || s.pos >= s.instr.length

  @tailrec
  def loop[A](s: A)(f: A => A, stop: A => Boolean): A = {
    val newS = f(s)
    if (stop(newS)) newS else loop(newS)(f, stop)
  }

  override def part1(lines: Iterator[String]) : Int = {
    val state0 = State(lines.map(_.toInt).toIndexedSeq, 0, 0)
    loop(state0)(next(_ => 1), stop).count
  }

  override def part2(lines: Iterator[String]) : Int = {
    val state0 = State(lines.map(_.toInt).toIndexedSeq, 0, 0)
    loop(state0)(next(i => if (i >= 3) -1 else 1), stop).count
  }
}
