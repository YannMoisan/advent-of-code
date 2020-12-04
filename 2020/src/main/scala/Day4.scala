import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// It is possible to traverse only once the input
// The solution traverse multiple times to make the code cleaner by splitting concerns
object Day4 extends MultiPuzzle[Int, Int] {
  type Document = Map[String, String]
  override def part1(input: Iterator[String]): Int =
    split(input.toList, "").map(toDocument).count(isValid1)

  override def part2(input: Iterator[String]): Int =
    split(input.toList, "").map(toDocument).count(isValid2)

  private def isValid1(document: Document): Boolean =
    List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").forall(document.contains)

  private def isValid2(document: Document): Boolean = {
    val a = document.get("byr").flatMap(_.toIntOption).exists(byr => byr >= 1920 && byr <= 2002)
    val b = document.get("iyr").flatMap(_.toIntOption).exists(byr => byr >= 2010 && byr <= 2020)
    val c = document.get("eyr").flatMap(_.toIntOption).exists(byr => byr >= 2020 && byr <= 2030)
    val d = document.get("hgt").exists { hgt =>
      hgt match {
        case s"${i}cm" => i.toIntOption.exists(i => i >= 150 && i <= 193)
        case s"${i}in" => i.toIntOption.exists(i => i >= 59 && i <= 76)
        case _         => false
      }
    }
    val e = document.get("hcl").exists { hcl =>
      hcl match {
        case s"#$i" => i.forall(c => c.isDigit || Set('a', 'b', 'c', 'd', 'e', 'f').contains(c))
        case _      => false

      }
    }
    val f = document
      .get("ecl").exists(ecl => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl))
    val g = document.get("pid").exists(pid => (pid.length == 9) && pid.forall(_.isDigit))
    a && b && c && d && e && f && g
  }

  // split(List("A", "B", ";", "C")) should return List(List("A", "B"), List("C"))
  private def split(lines: List[String], sep: String): List[List[String]] = {
    val parent = new ListBuffer[List[String]]
    val child  = new ListBuffer[String]
    lines.foreach { line =>
      if (line != sep) {
        val _ = child.addOne(line)
      } else {
        val _ = parent.addOne(child.toList)
        child.clear()
      }
    }
    val _ = parent.addOne(child.toList)
    parent.toList
  }

  private def toDocument(lines: List[String]): Document = {
    val map = mutable.Map[String, String]()
    lines.foreach { line =>
      val kvs = line.split(' ')
      kvs.foreach { kv =>
        val Array(k, v) = kv.split(':')
        map(k) = v
      }
    }
    map.toMap
  }
}
