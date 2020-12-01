object Day6 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    //println(input.map(parse).take(5).mkString(","))

    val grid = Array.fill(1000)(Array.fill(1000)(0))

    input.foreach { s =>
      val action = parse(s)
      action match {
        case TurnOn(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = 1
        case TurnOff(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = 0
        case Toggle(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = if (grid(y)(x) == 1) 0 else 1
      }
    }

    var sum = 0

    for {
      x <- 0 to 999
      y <- 0 to 999
    } if (grid(y)(x) == 1) sum += 1

    sum
  }

  // Starting Scala 2.13, as an alternative to regex solutions,
  // it's also possible to pattern match a String by unapplying a string interpolator:
  def parse(s: String) = s match {
    case s"turn on $xa,$ya through $xb,$yb"  => TurnOn(xa.toInt, ya.toInt, xb.toInt, yb.toInt)
    case s"turn off $xa,$ya through $xb,$yb" => TurnOff(xa.toInt, ya.toInt, xb.toInt, yb.toInt)
    case s"toggle $xa,$ya through $xb,$yb"   => Toggle(xa.toInt, ya.toInt, xb.toInt, yb.toInt)
  }

  override def part2(input: Iterator[String]): Int = {
    //println(input.map(parse).take(5).mkString(","))

    val grid = Array.fill(1000)(Array.fill(1000)(0))

    input.foreach { s =>
      val action = parse(s)
      action match {
        case TurnOn(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = grid(y)(x) + 1
        case TurnOff(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = math.max(0, grid(y)(x) - 1)
        case Toggle(xa, ya, xb, yb) =>
          for {
            x <- xa to xb
            y <- ya to yb
          } grid(y)(x) = grid(y)(x) + 2
      }
    }

    var sum = 0

    for {
      x <- 0 to 999
      y <- 0 to 999
    } sum += grid(y)(x)

    sum
  }

}

sealed trait Action                                    extends Product with Serializable
case class TurnOn(xa: Int, ya: Int, xb: Int, yb: Int)  extends Action
case class TurnOff(xa: Int, ya: Int, xb: Int, yb: Int) extends Action
case class Toggle(xa: Int, ya: Int, xb: Int, yb: Int)  extends Action
