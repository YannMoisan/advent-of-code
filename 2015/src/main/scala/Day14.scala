import scala.collection.mutable

// label: division
object Day14 extends MultiPuzzle[Int, Int] {
  case class Reindeer(name: String, speed: Int, flyDuration: Int, restDuration: Int)

  override def part1(input: Iterator[String]): Int = {
    val reindeers: Seq[Reindeer] = input.map { line =>
      val s"$name can fly $speed km/s for $flyDuration seconds, but then must rest for $restDuration seconds." =
        line
      Reindeer(name, speed.toInt, flyDuration.toInt, restDuration.toInt)
    }.toList

    val limit = 2503

    reindeers.map { r =>
      val quotient  = limit / (r.flyDuration + r.restDuration)
      val remainder = limit % (r.flyDuration + r.restDuration)
      quotient * r.flyDuration * r.speed + math.min(remainder, r.flyDuration) * r.speed
    }.max
    // 882 too low
  }

  override def part2(input: Iterator[String]): Int = {
    val reindeers: Seq[Reindeer] = input.map { line =>
      val s"$name can fly $speed km/s for $flyDuration seconds, but then must rest for $restDuration seconds." =
        line
      Reindeer(name, speed.toInt, flyDuration.toInt, restDuration.toInt)
    }.toList

    val scores = mutable.Map[String, Int]()
    val dist   = mutable.Map[String, Int]()

    (0 until 2503).foreach { i =>
      reindeers.foreach { r =>
        val remainder = i % (r.flyDuration + r.restDuration)
        dist.updateWith(r.name)(opt =>
          Some(opt.getOrElse(0) + (if (remainder < r.flyDuration) r.speed else 0))
        )
      }
      val max = dist.maxBy { case (_, v) => v }._2
      dist.foreach {
        case (k, v) =>
          if (v == max) {
            scores.updateWith(k)(o => Some(o.getOrElse(0) + 1))
          }
      }
    }
    println(dist)
    println(scores)
    scores.maxBy { case (_, v) => v }._2
  }

}
//           1
// 0123456789012345
// XXOOOXXOOOXXOOO
