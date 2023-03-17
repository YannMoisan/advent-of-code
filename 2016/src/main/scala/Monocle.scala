import monocle.Lens
import monocle.function.Index.index
import monocle.macros.{GenLens, Lenses}

object Monocle extends App {

  @Lenses
  case class Pos(x: Int, y: Int)

  def moveX1 = (p: Pos) => Pos(p.x + 1, p.y)

  def moveX2 = (p: Pos) => p.copy(x = p.x + 1)

  def moveX3 = Pos.x.modify(_ + 1)

  @Lenses
  case class State(mem: Map[String, Int], pointer: Int)

  def add1 = (s: State) => s.copy(mem = s.mem.updated("a", s.mem("a") + 1), pointer = s.pointer + 1)

  val mem: Lens[State, Map[String, Int]] = GenLens[State](_.mem)
  val pointer                            = GenLens[State](_.pointer)

  val add2 = (mem composeOptional index("a")).modify(_ + 1) andThen pointer.modify(_ + 1)

//  def combine[S, A, B](lsa : Lens[S, A], f: A => A, lsb: Lens[S, B], g: B => B) : S => S = { s =>
//    val a = lsa.get(s)
//    val b = lsb.get(s)
//    val s2 = lsa.set(f(a))
//    val s3 = lsb.set(g(b))
//    s2(s3(s))
//  }

  def mergeLens[S, A, B](lsa: Lens[S, A], lsb: Lens[S, B]): Lens[S, (A, B)] =
    Lens.apply[S, (A, B)](s => (lsa.get(s), lsb.get(s)))(t => (lsa.set(t._1) andThen lsb.set(t._2)))

  def combine[S, A, B](lsa: Lens[S, A], f: A => A, lsb: Lens[S, B], g: B => B): S => S =
    mergeLens(lsa, lsb).modify { case (a, b) => (f(a), g(b)) }

  @Lenses
  case class Values(a: Int, b: Int)

  def add3 = combine(Values.a, (_: Int) + 1, Values.b, (_: Int) + 1)

//    val a = lsa.get(s)
//    val b = lsb.get(s)
//    val s2 = lsa.set(f(a))
//    val s3 = lsb.set(g(b))
//    s2(s3(s))

  //composeLens mpointer
  //def add = State.mem.at("a")

  println(moveX1(Pos(0, 0)))
  println(moveX2(Pos(0, 0)))
  println(moveX3(Pos(0, 0)))

  println(add1(State(Map("a" -> 2), 5)))
  println(add2(State(Map("a" -> 2), 5)))

  println(add3(Values(2, 3)))

//  println(add1(State(Map(), 5)))
//  println(add2(State(Map(), 5)))

  println(add1(State(Map().withDefaultValue(0), 5)))
  println(add2(State(Map().withDefaultValue(0), 5)))

}
