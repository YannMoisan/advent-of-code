import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import scala.collection.mutable

object Day4 extends MultiPuzzle[Int, Int] {
  private val BeginsShift = """\[(.*)\] Guard #(\d+) begins shift""".r
  private val FallsAsleep = """\[(.*)\] falls asleep""".r
  private val WakesUp = """\[(.*)\] wakes up""".r
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  override def part1: Iterator[String] => Int = { iter =>
    val asleepByGuard = computeMinutesAsleepByGuard(iter)
    val winner = asleepByGuard.toSeq.maxBy(_._2.length)
    val mostFreqMinute = winner._2.groupBy(identity).maxBy(_._2.length)._1
    winner._1 * mostFreqMinute
  }

  def parse(line: String): (LocalDateTime, Int) = {
    line match {
      case BeginsShift(ts, id) => (LocalDateTime.parse(ts, formatter), id.toInt)
      case FallsAsleep(ts)     => (LocalDateTime.parse(ts, formatter), -1)
      case WakesUp(ts)         => (LocalDateTime.parse(ts, formatter), -2)
    }
  }
  override def part2: Iterator[String] => Int = { iter =>
    val asleepByGuard = computeMinutesAsleepByGuard(iter)
    val tmp =
      asleepByGuard.mapValues(_.groupBy(identity).toSeq.maxBy(_._2.length))
    val winner = tmp.toSeq.maxBy(_._2._2.length)
    winner._1 * winner._2._1
  }

  def computeMinutesAsleepByGuard(
      iter: Iterator[String]): mutable.Map[Int, Seq[Int]] = {
    val lines: Array[String] = iter.toArray
    val parsed = lines.map(parse)
    val ordered: Array[(LocalDateTime, Int)] =
      parsed.sortBy(_._1.toEpochSecond(ZoneOffset.UTC))
    val asleepByGuard = mutable.Map[Int, Seq[Int]]()
    var guard = 0
    var i = 0
    while (i < ordered.length) {
      val cur = ordered(i)
      if (cur._2 > 0) {
        guard = cur._2
        i += 1
      } else {
        val start = cur._1.getMinute
        val end = ordered(i + 1)._1.getMinute
        asleepByGuard(guard) = asleepByGuard.getOrElse(guard, Seq.empty) ++ (start until end)
        i += 2
      }
    }
    asleepByGuard
  }
}
