import scala.collection.mutable

//[N]     [C]                 [Q]
//[W]     [J] [L]             [J] [V]
//[F]     [N] [D]     [L]     [S] [W]
//[R] [S] [F] [G]     [R]     [V] [Z]
//[Z] [G] [Q] [C]     [W] [C] [F] [G]
//[S] [Q] [V] [P] [S] [F] [D] [R] [S]
//[M] [P] [R] [Z] [P] [D] [N] [N] [M]
//[D] [W] [W] [F] [T] [H] [Z] [W] [R]
// 1   2   3   4   5   6   7   8   9
object Day5 extends MultiPuzzle[String, String] {
  val arr = Array(
    mutable.Stack.apply("N", "W", "F", "R", "Z", "S", "M", "D"),
    mutable.Stack.apply("S", "G", "Q", "P", "W"),
    mutable.Stack.apply("C", "J", "N", "F", "Q", "V", "R", "W"),
    mutable.Stack.apply("L", "D", "G", "C", "P", "Z", "F"),
    mutable.Stack.apply("S", "P", "F"),
    mutable.Stack.apply("L", "R", "W", "F", "D", "H"),
    mutable.Stack.apply("C", "D","N", "Z"),
    mutable.Stack.apply("Q", "J", "S", "V", "F", "R", "N", "W"),
    mutable.Stack.apply("V", "W", "Z", "G", "S", "M", "R")
  )

  val arr2 = Array(
    mutable.Stack.apply("N", "W", "F", "R", "Z", "S", "M", "D"),
    mutable.Stack.apply("S", "G", "Q", "P", "W"),
    mutable.Stack.apply("C", "J", "N", "F", "Q", "V", "R", "W"),
    mutable.Stack.apply("L", "D", "G", "C", "P", "Z", "F"),
    mutable.Stack.apply("S", "P", "F"),
    mutable.Stack.apply("L", "R", "W", "F", "D", "H"),
    mutable.Stack.apply("C", "D", "N", "Z"),
    mutable.Stack.apply("Q", "J", "S", "V", "F", "R", "N", "W"),
    mutable.Stack.apply("V", "W", "Z", "G", "S", "M", "R")
  )

  override def part1(input: Iterator[String]): String = {
    input.drop(10).foreach { s=>
      val s"move $count from $from to $to" = s
      (1 to count.toInt).foreach {
        _ =>
          val e = arr(from.toInt - 1).pop()
          arr(to.toInt - 1).push(e)
      }
    }
    (1 to 9).map{ i => arr(i-1).pop()}.mkString
  }

  override def part2(input: Iterator[String]): String = {
    input.drop(10).foreach { s =>
      val s"move $count from $from to $to" = s
      val l: Seq[String] = (1 to count.toInt).map {
        _ => arr2(from.toInt - 1).pop()
      }
      l.reverse.foreach(arr2(to.toInt - 1).push(_))
    }
    (1 to 9).map { i => arr2(i - 1).pop() }.mkString
  }
}
