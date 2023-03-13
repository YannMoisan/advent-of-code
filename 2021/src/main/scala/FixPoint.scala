package fp

// Given a function f : A => A and a start value
// Determine the number of iterations before f converges i.e. f(a) = a
// https://en.wikipedia.org/wiki/Fixed_point_(mathematics)
trait FixPoint[A] {
  def convergesIn(f: A => A, start: A, pred: (A, A) => Boolean): Int
}

class Iterative[A] extends FixPoint[A] {
  override def convergesIn(f: A => A, start: A, pred: (A, A) => Boolean): Int = {
    var cur   = start
    var i     = 0
    var found = false
    while (!found) {
      val next = f(cur)
      if (pred(next, cur)) {
        found = true
      } else {
        cur = next
        i += 1
      }
    }
    i
  }
}

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class Functional[A] extends FixPoint[A] {
  override def convergesIn(f: A => A, start: A, pred: (A, A) => Boolean): Int =
    Iterator
      .iterate(start)(f)
      .sliding(2)
      .zipWithIndex
      .collectFirst { case (seq, index) if pred(seq(0), seq(1)) => index }
      .get
}

object FixPointApp extends App {
  val f: Int => Int = _ / 2
  val start         = 8
  // 8 => 4 => 2 => 1 => 0 => 0

  println(new Iterative[Int]().convergesIn(f, start, _ == _))
  println(new Functional[Int]().convergesIn(f, start, _ == _))
}
