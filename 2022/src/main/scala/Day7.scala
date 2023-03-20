import scala.collection.mutable

object Day7 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val paths = parse(input)
    paths
      .map {
        case (currentDir, v) =>
          v + paths.collect { case (k, v2) if k.startsWith(currentDir + "/") => v2 }.sum
      }.filter(_ <= 100000).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val paths = parse(input)
    val totalSizes = paths.map {
      case (currentDir, v) =>
        v + paths.collect { case (k, v2) if k.startsWith(currentDir + "/") => v2 }.sum
    }
    val used     = 43313415
    val free     = 70000000 - used
    val required = 30000000
    val missing  = required - free

    totalSizes.toList.sorted.find(_ >= missing).get
  }

  private def parse(input: Iterator[String]): Map[String, Int] = {
    var workingDir = "/root"
    val m          = mutable.Map[String, Int]()
    input.foreach {
      _ match {
        case s"$$ cd /"  => workingDir = "/root"
        case s"$$ cd .." => workingDir = workingDir.substring(0, workingDir.lastIndexOf("/"))
        case s"$$ cd $dir" =>
          val _ = m.updateWith(workingDir)(i => Some(i.getOrElse(0)))
          workingDir = workingDir + "/" + dir
        case s"dir $_"   =>
        case s"$$ ls"    =>
        case s"$size $_" => val _ = m.updateWith(workingDir)(i => Some(i.getOrElse(0) + size.toInt))
      }
    }
    m.toMap
  }
}
