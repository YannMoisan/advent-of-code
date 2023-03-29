import scala.collection.mutable.ArrayBuffer

object Day19 extends MultiPuzzle[Int, Int] {

  // List(A,B,C) A => C,D
  // List(A,B,C)

  override def part1(input: Iterator[String]): Int = {
    val list = input.toList
    val transfo: Seq[(String, String)] = list.dropRight(2).map {
      case s"$src => $dst" => (src, dst)
    }
    val molecule = list.last
    val split    = splitCamelCase(molecule)
    val _ = split.foldLeft(0L) {
      case (acc, token) =>
        val occ = transfo.count(x => x._1 == token)
        acc + occ
    }

    val replacements: Seq[Array[String]] = oneReplace(split.toArray, transfo)
    replacements.map(_.mkString).toSet.size
  }

  override def part2(input: Iterator[String]): Int =
    42

  // TODO Generalize to split any list given a predicate
  def splitCamelCase2(s: String): Seq[String] =
    s.split("(?<!^)(?=[A-Z])").toList

  // TODO Generalize to split any list given a predicate
  def splitCamelCase(s: String): Seq[String] = {
    var candidate = s(0).toString
    val res       = ArrayBuffer[String]()
    var pos       = 1
    while (pos < s.length) {
      if (s(pos).isLower) {
        candidate += s(pos)
        pos += 1
      } else {
        val _ = res.addOne(candidate)
        candidate = s(pos).toString
        pos += 1
      }
    }
    // TODO avoid duplication for last token
    val _ = res.addOne(candidate)
    res.toList
  }

  def oneReplace[A](l: Array[A], replacements: Seq[(A, A)]): Seq[Array[A]] =
    (0 until l.size).flatMap { i =>
      if (replacements.exists(x => x._1 == l(i))) {
        val t: Seq[Array[A]] = replacements.filter(x => x._1 == l(i)).map { x =>
          val res = l.clone()
          res(i) = x._2
          res
        }
        t
      } else Seq()
    }
}
