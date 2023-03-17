import cats.instances.all._
import com.yannmoisan.util.fp.{loop, loop2}

object Rec extends App {

  //https://en.wikipedia.org/wiki/Caesar_cipher
  def shift1[A](s: Seq[A]): A => A = {
    val shifted = s.tail :+ s.head
    val m       = s.zip(shifted).toMap
    a => m(a)
  }

  def shiftN[A](s: Seq[A], nb: Int): A => A =
    a => s((s.indexOf(a) + nb) % s.length)

  val shifter = shift1('a' to 'z')
  println(shifter('a'))
  println(shifter('z'))

  println(shiftN('a' to 'z', 2)('a'))
  println(shiftN('a' to 'z', 2)('b'))
  println(shiftN('a' to 'z', 2)('y'))
  println(shiftN('a' to 'z', 2)('z'))

  println(loop(0)(_ + 1, _ >= 10))
  println(loop2(0)(_ + 1, _ >= 10))

  def g[I, S](is: Seq[I], f: I => S => S, init: S): S =
    catsStdMonoidKForFunction1.algebra.combineAll(is.map(f))(init)

  val g2 = g(List("a", "b", "c"), (i: String) => (s: String) => s + i, "foo")

  println(g2)

}
