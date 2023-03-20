import scala.collection.mutable

trait Element

case class I(i: Int) extends Element

case class L(l: mutable.Buffer[Element]) extends Element

object Day13 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input
      .grouped(3).zipWithIndex.collect {
        case ((Seq(fst, snd, _), i)) if compare(fst, snd) == -1 => i + 1
        case ((Seq(fst, snd), i)) if compare(fst, snd) == -1    => i + 1
      }.sum

  override def part2(input: Iterator[String]): Int = {
    val l      = input.toList.filter(_ != "").++(Seq("[[2]]", "[[6]]"))
    val sorted = l.sortWith(compare(_, _) < 0)
    (1 + sorted.indexOf("[[2]]")) * (1 + sorted.indexOf("[[6]]"))
  }

  def compare(a: String, b: String): Int = compare(parse(a), parse(b))

  def compare(a: Element, b: Element): Int =
    //println(s"compare $a with $b")
    (a, b) match {
      case (I(aa), I(bb)) => if (aa == bb) 0 else if (aa < bb) -1 else 1
      case (L(aa), I(bb)) => compare(L(aa), L(mutable.Buffer(I(bb))))
      case (I(aa), L(bb)) => compare(L(mutable.Buffer(I(aa))), L(bb))
      case (L(aa), L(bb)) =>
        var res = 0
        var i   = 0
        while (res == 0 && i < math.min(aa.length, bb.length)) {
          val c = compare(aa(i), bb(i))
          if (c == 0) { i += 1 }
          else res = c
        }
        if (res == 0 && aa.length != bb.length) {
          res = Integer.compare(aa.length, bb.length)
        }
        res
    }

  def parse(s: String): L = {
    val stack = mutable.Stack[L]()
    val res   = L(mutable.Buffer())
    val _     = stack.push(res)
    s.substring(1)
      .foreach { c =>
        c match {
          case '[' =>
            val n = L(mutable.Buffer())
            val _ = stack.top.l += n
            val _ = stack.push(n)
          case ']' => stack.pop()
          case ',' =>
          case 'a' =>
            val l = stack.top
            l.l += I(10)
          case i =>
            val l = stack.top
            l.l += I(i.toString.toInt)
        }
      }
    res
  }
}
