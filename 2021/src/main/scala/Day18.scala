object Day18 extends MultiPuzzle[Int, Int] with App {
  trait Element
  case class Value(v: Int)                               extends Element
  case class Pair(var left: Element, var right: Element) extends Element

  override def part1(input: Iterator[String]) = {
    val res = add(input.toSeq)
    println(parse(res))

    println("res:" + res)

    magnitude(parse(res))

    //[[[[0,6],[7,7]],[[7,8],[6,8]]],[[[8,7],[8,8]],[[1,9],[2,2]]]]
    //[[[12,35],[37,34]],[[38, 40],[21,10]]]
    //[[106,179],[194, 83]]
    //[676,748]
    //3524
  }

  override def part2(input: Iterator[String]) = {
    val lines = input.toSeq
    (for {
      i <- 0 until lines.length
      j <- 0 until lines.length
      if i != j
    } yield {
      magnitude(parse(add(lines(i), lines(j))))
    }).max
  }

  def magnitude(e: Element): Int =
    e match {
      case Value(v)          => v
      case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
    }

  def parse(s: String): Element = {
    var root: Pair = null
    var stack      = List.empty[Pair]
    s.foreach { ch =>
//      println(s"ch=$ch")
      ch match {
        case '[' =>
          val p = new Pair(null, null)
          if (root == null) {
            root = p
            stack = List(root)
          } else {
            if (stack.head.left == null)
              stack.head.left = p
            else stack.head.right = p
            stack = p :: stack
          }
        case ']' =>
          stack = stack.tail
        case ',' =>
        case i =>
          val v = Value(i.toString.toInt)
          if (stack.head.left == null)
            stack.head.left = v
          else stack.head.right = v
      }
    }
    root
  }

//  println(parse("[1,2]"))
//  println(parse("[[1,2],3]"))
//  println(parse("[9,[8,7]]"))
//  println(parse("[[1,9],[8,5]]"))
//  println(parse("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"))

  // [[[[[9,8],1],2],3],4]

  def toExplode(s: String): Option[(Int, Int, Int, Int)] = {
    var depth = 0
    var start = -1
    var end   = -1
    (0 until s.length).foreach { i =>
      if (s(i) == '[') {
        depth += 1
        if (depth == 5 && start == -1)
          start = i
      }
      if (s(i) == ']') {
        if (depth == 5 && end == -1)
          end = i
        depth -= 1
      }
    //println(s"${s(i)}:depth=$depth")
    }

    if (start == -1)
      None
    else {
      val Array(a, b) = s.substring(start + 1, end).split(",")
      Some((start, end, a.toInt, b.toInt))
    }
  }

  def prev(s: String, start: Int, end: Int): (Option[Int], Option[Int]) = {
    val prev = (start to 0).by(-1).find(i => ('0' to '9').contains(s(i)))
    val next = (end until s.length).find(i => ('0' to '9').contains(s(i)))
    (prev, next)
  }

  def toto(s: String, start: Int): Option[(Int, Int)] = {
    val prev = (start to 0).by(-1).find(i => ('0' to '9').contains(s(i)))
    prev match {
      case Some(i) =>
        Some(((i - 1 to 0).by(-1).find(i => Seq('[', ',', ']').contains(s(i))).get + 1, i))
      case None =>
        None
    }
  }

  def tata(s: String, end: Int): Option[(Int, Int)] = {
    val next = (end until s.length).find(i => ('0' to '9').contains(s(i)))
    next match {
      case Some(i) =>
        Some((i, (i + 1 until s.length).find(i => Seq('[', ',', ']').contains(s(i))).get - 1))
      case None =>
        None
    }
  }

  //            012345678
  println(toto("[233,345],[23,12]", 8))
  println(tata("[233,345],[23,12]", 4))
  // [233,45],[23,12]

  def explode(s: String) =
    toExplode(s) match {
      case None => s
      case Some((start, end, a, b)) =>
        val pre = toto(s, start)
        //val (pre, next) = prev(s, start, end)

        val s2: String = if (pre.isDefined) {
          val newleft = s.substring(pre.get._1, pre.get._2 + 1).toInt + a
          println(s"new left:$newleft")
          s.substring(0, pre.get._1) + newleft + s.substring(pre.get._2 + 1)
        } else {
          s
        }

        val next = tata(s2, end + s2.length - s.length)
        val s3 = if (next.isDefined) {
          val newright = s2.substring(next.get._1, next.get._2 + 1).toInt + b
          println(s"new right:$newright")
          s2.substring(0, next.get._1) + newright + s2.substring(next.get._2 + 1)
        } else {
          s2
        }
        s3.substring(0, start + s2.length - s.length) + "0" + s3.substring(
          end + 1 + s2.length - s.length
        )
    }

  def split(s: String): String =
    // find the first number > 10
    (0 until s.length - 1)
      .find(i => ('0' to '9').contains(s(i)) && ('0' to '9').contains(s(i + 1))) match {
      case None => s
      case Some(i) =>
        val n = s.substring(i, i + 2).toInt
        s.substring(0, i) + "[" + n / 2 + "," + (n - n / 2) + "]" + s.substring(i + 2)
    }

  def explodeOrSplit(s: String): String = {
    val s2 = explode(s)
    if (s == s2) {
      split(s2)
    } else
      s2
  }

  def reduce(s: String): String = {
    var prev: Option[String] = None
    var cur                  = s
    while (!prev.exists(_ == cur)) {
      prev = Some(cur)
      cur = explodeOrSplit(cur)
      println(cur)
    }
    cur
  }

  def add(a: String, b: String) = reduce(s"[$a,$b]")

  def add(lines: Seq[String]): String =
    lines.tail.foldLeft(lines.head) { case (acc, a) => add(acc, a) }

  //[info]   "
  // [[[[0,7],4],[15,[0],9]]]],[1,1]]" did not equal "
  // [[[[0,7],4],[15,[[0,13]]]],[1,1]]" (Day18Spec.scala:32)

  println(toExplode("[[[[[9,8],1],2],3],4]"))
  println("[[[[[9,8],1],2],3],4]".substring(4, 8 + 1))

  println(prev("[[[[[9,8],1],2],3],4]", 4, 8))
  println(explode("[[[[[9,8],1],2],3],4]"))
//  [1,2]
//  [[1,2],3]
//  [9,[8,7]]
//  [[1,9],[8,5]]
//  [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
//  [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
//  [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
}
