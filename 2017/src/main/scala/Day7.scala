import scala.annotation.tailrec

object Day7 extends MultiPuzzle[String, Unit] {

  val pattern = """(\w+) \((\d+)\)(?: -> (.*))*""".r

  final case class Line(word: String, weight: Int, words: Seq[String])

  def w(map: Map[String, Line], e: String): Int = {
    val cur = map(e)
    val childW = cur.words.map(w(map, _))
    println(s"cur='$cur', children=${childW.mkString(":")}")
    cur.weight + childW.sum
  }

  @tailrec
  def lookup(map: Map[String, String], e: String): String = {
    map.get(e) match {
      case None    => e
      case Some(s) => lookup(map, s)
    }
  }

  override def part1(llines: Iterator[String]) : String = {
    val lines = llines.collect {
      case pattern(word, weight, words) =>
        Line(word,
             Integer.parseInt(weight),
             if (words != null) words.split(", ").toSeq else Seq.empty[String])
    }

    val child2Parent = lines.foldLeft(Map.empty[String, String]) {
      case (m, lines) => m ++ lines.words.map(_ -> lines.word)
    }

//    val parent2Child = lines.map(line => line.word -> line).toMap

    lookup(child2Parent,
           child2Parent.headOption
             .map(_._1)
             .getOrElse(""))

  }

  override def part2(llines: Iterator[String]) : Unit = {
//    val root = lookup(child2Parent, child2Parent.head._1)
//    println(w(parent2Child, root))
  }

}
