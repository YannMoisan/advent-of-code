import scala.collection.mutable

object Day24 extends MultiPuzzle[Int, Int] {
  // 99911993949684

  trait Instruction
  final case class Inp(a: String)            extends Instruction
  final case class Add(a: String, b: String) extends Instruction
  final case class Mul(a: String, b: String) extends Instruction
  final case class Div(a: String, b: String) extends Instruction
  final case class Mod(a: String, b: String) extends Instruction
  final case class Eql(a: String, b: String) extends Instruction

  // z.push(A+6)
  // z.push(B+6)
  // z.push(C+3)
  // if D != z.pop -11
  //   z.push(D+11)
  // z.push(E+9)
  // if F != z.pop -1
  //   z.push(F+3)
  // z.push(G+13)
  // z.push(H+6)
  // if I != z.pop - 0
  //   z.push(I+14)
  // z.push(J+10)
  // if K != z.pop - 5
  //   z.push(K-12)
  // if L != z.pop - 16
  //   z.push(L-10)
  // if M != z.pop - 7
  //   z.push(M-11)
  // if N != z.pop - 11
  //   z.push(M-15)

  // D = C - 8
  // F = E + 8
  // I = H + 6
  // K = J + 5
  // L = G -3
  // M = B - 1
  // N = A - 5

// ABCDEFGHIJKLMN
// 99911993949684
// 62911941716111

  //
  //
  // z.push()
  // z.push()
  // z.push()
  // z.push()

  override def part1(input: Iterator[String]): Int = {
    val instructions = input.map(parse).toSeq
//119119117
//119119
// xx9119xxxxxxxxxxxx

    // a tester
    // 1111716
    // 11911911716

    // 11911911716

    // 119119417161

    //Vector((1919117,4928), (1919128,4928), (1919139,4928), (1919217,4929),
    // (1919228,4929), (1919239,4929), (1919317,4930), (1919328,4930), (1919339,4930),
    // (1919417,4931), (1919428,4931), (1919439,4931), (1919517,4932), (1919528,4932), (1919539,4932), (1919617,4933), (1919628,4933), (1919639,4933), (1919717,4934), (1919728,4934), (1919739,4934), (1919817,4935), (1919828,4935), (1919839,4935), (1919917,4936), (1919928,4936), (1919939,4936), (2919

    // 11911
    val min = (111111111 to 999999999)
      .map(_.toString).filter(!_.contains("0"))
      .map { i =>
        val v = mutable.Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L)
        val _ = execute(
          i.substring(0, 2) + "9119" + i.substring(2),
          v,
          instructions
        )
        i -> v("z")
      }.filter(_._2 < 500)
    println(min)
    42
  }

  override def part2(input: Iterator[String]): Int = 42

  def parse(line: String): Instruction =
    line match {
      case s"inp $a"    => Inp(a)
      case s"add $a $b" => Add(a, b)
      case s"mul $a $b" => Mul(a, b)
      case s"div $a $b" => Div(a, b)
      case s"mod $a $b" => Mod(a, b)
      case s"eql $a $b" => Eql(a, b)
    }

  def execute(init: String, v: mutable.Map[String, Long], functions: Seq[Instruction]): Long = {
    var i = 0
    functions.take(18 * init.length).foreach { f =>
      //println(s"$v - $f")
      f match {
        case Inp(a) =>
          v(a) = init(i).toString.toLong
          i += 1
        case Add(a, b) => v(a) = v(a) + v.getOrElse(b, b.toLong)
        case Mul(a, b) => v(a) = v(a) * v.getOrElse(b, b.toLong)
        case Div(a, b) => v(a) = v(a) / v.getOrElse(b, b.toLong)
        case Mod(a, b) => v(a) = v(a) % v.getOrElse(b, b.toLong)
        case Eql(a, b) => v(a) = if (v(a) == v.getOrElse(b, b.toLong)) 1L else 0L
      }
    }
    v("z")
  }
}
