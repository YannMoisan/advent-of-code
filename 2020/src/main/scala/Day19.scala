import scala.collection.mutable

object Day19 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val unparsedRule: List[String] = input.takeWhile(_ != "").toList
    val regex                      = generateRegex(unparsedRule).r

    val messages = input.toList
    messages.count(msg => regex.matches(msg))
  }

  override def part2(input: Iterator[String]): Int = {
    val unparsedRule: List[String] = input.takeWhile(_ != "").toList
    val regexes                    = generateRegexes(unparsedRule).map(_.r)

    val messages = input.toList
    messages.count(msg => regexes.exists(_.matches(msg)))
  }

  def generateRegex(rules: List[String]): String = {
    // TODO : improve with DFS
    val m = mutable.Map[String, String]()
    while (!m.contains("0")) {
      rules.foreach {
        case s"$id: $a $b | $c $d" =>
          if (m.contains(a) && m.contains(b) && m.contains(c) && m.contains(d))
            m(id) = s"(${m(a)}${m(b)}|${m(c)}${m(d)})"
        case s"$id: $a | $b" => if (m.contains(a) && m.contains(b)) m(id) = s"(${m(a)}|${m(b)})"
        case s"$id: $a $b"   => if (m.contains(a) && m.contains(b)) m(id) = s"${m(a)}${m(b)}"
        case s"""$id: "a"""" => m(id) = "a"
        case s"""$id: "b"""" => m(id) = "b"
        case s"$id: $a"      => if (m.contains(a)) m(id) = m(a)
      }
    }

//    m.toList.sortBy(_._1.toInt).foreach(kv => println(kv))
//    println(m("0"))
    println("42:" + m("42"))
    println("31:" + m("31"))

    m("0")
  }

  def generateRegexes(rules: List[String]): Seq[String] = {
    // TODO : improve with DFS
    val m = mutable.Map[String, String]()
    while (!m.contains("0")) {
      rules.foreach {
        case s"$id: $a $b | $c $d" =>
          if (m.contains(a) && m.contains(b) && m.contains(c) && m.contains(d))
            m(id) = s"(${m(a)}${m(b)}|${m(c)}${m(d)})"
        case s"$id: $a | $b" => if (m.contains(a) && m.contains(b)) m(id) = s"(${m(a)}|${m(b)})"
        case s"$id: $a $b"   => if (m.contains(a) && m.contains(b)) m(id) = s"${m(a)}${m(b)}"
        case s"""$id: "a"""" => m(id) = "a"
        case s"""$id: "b"""" => m(id) = "b"
        case s"$id: $a"      => if (m.contains(a)) m(id) = m(a)
      }
    }

    //    m.toList.sortBy(_._1.toInt).foreach(kv => println(kv))
    //    println(m("0"))
    println("42:" + m("42"))
    println("31:" + m("31"))

    (1 to 10).map(i => s"(${m("42")})+${m("42")}{$i}${m("31")}{$i}")
  }

}
