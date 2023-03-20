import scala.collection.mutable

object Day19 extends MultiPuzzle[Int, Int] {
  case class Scanner(pos: Set[Pos])
  case class Pos(c: (Int, Int, Int))

  override def part1(input: Iterator[String]): Int = {
    val scanners = mutable.Buffer[Array[String]]()
    var cur      = mutable.Buffer[String]()
    input.foreach { line =>
      if (line.isEmpty) {
        val _ = scanners.append(cur.toArray)
        cur = mutable.Buffer[String]()
      } else {
        val _ = cur.append(line)
      }
    }
    val _ = scanners.append(cur.toArray)

    scanners.foreach(scanner => println(scanner.head))

    val sc: Array[Scanner] =
      scanners.map(arr => Scanner(arr.drop(1).toIndexedSeq.map(parse).toSet)).toArray
    println(sc.size)

    rotate(Pos((1, 2, 3))).foreach(println(_))

    var i      = 1
    val merged = mutable.Set[Int]()
    val vects  = mutable.Set[(Int, Int, Int)]()
    while (merged.size != sc.size - 1) {
      rotateScanner(sc(i)).foreach { rotatedScanner =>
        val vecs = overlap(sc(0), rotatedScanner)
        if (!vecs.isEmpty) {
          println("merge:" + i)
          sc(0) = merge(sc(0), rotatedScanner, vecs.head)
          val _ = vects.add(vecs.head)
          merged.add(i)
        }
      }
      i += 1
      if (i >= sc.size)
        i = 1
    }

    // for part2
    println("max:" + vects.toList.combinations(2).maxBy {
      case (a, b, c) :: (d, e, f) :: Nil => math.abs(a - d) + math.abs(b - e) + math.abs(c - f)
      case _                             => throw new IllegalStateException()
    })

    sc(0).pos.size

    //p => println(p.c))
    // pour chaque rot 24
    // pour chaque paire : 25 * 25 -> 1 vecteur
    // un Set point, un Set
    // on compte les match
  }

  override def part2(input: Iterator[String]): Int = 42

  def merge(a: Scanner, b: Scanner, vec: (Int, Int, Int)): Scanner =
    Scanner(
      a.pos ++ b.pos.map(p => Pos((p.c._1 - vec._1, p.c._2 - vec._2, p.c._3 - vec._3)))
    )

  // returns the vector when 12+ beacons overlap
  def overlap(a: Scanner, b: Scanner): Set[(Int, Int, Int)] = {
    // if a and b are the same beacon, vector is between s0 and s1
    val vectors = for {
      pa <- a.pos
      pb <- b.pos
    } yield (pb.c._1 - pa.c._1, pb.c._2 - pa.c._2, pb.c._3 - pa.c._3)

    vectors.filter { v =>
      val bbeacons = b.pos.map { pos =>
        Pos(
          (
            pos.c._1 - v._1,
            pos.c._2 - v._2,
            pos.c._3 - v._3
          )
        )
      }
      a.pos.intersect(bbeacons).size >= 12
    }
  }

  def parse(s: String): Pos = {
    val s"$x,$y,$z" = s
    Pos((x.toInt, y.toInt, z.toInt))
  }

  def rotateScanner(s: Scanner): Seq[Scanner] = {
    val tmp: Set[Seq[Pos]] = s.pos.map(rotate)
    tmp.transpose.map(Scanner.apply).toSeq
  }

  // https://gist.github.com/jkseppan/11f40e2866460515067bca7ebcfabb0d
  def rotate(p: Pos): Seq[Pos] = {
    val pp = Array((0, 1, 2), (1, 2, 0), (2, 0, 1))
    val sp = Array((1, 1, 1), (-1, -1, 1), (-1, 1, -1), (1, -1, -1))
    val pn = Array((0, 2, 1), (1, 0, 2), (2, 1, 0))
    val sn = Array((-1, 1, 1), (1, -1, 1), (1, 1, -1), (-1, -1, -1))
    ((for {
      epp <- pp
      esp <- sp
    } yield (epp, esp)) ++ (for {
      epn <- pn
      esn <- sn
    } yield (epn, esn))).toIndexedSeq.map {
      case (perm, sign) =>
        Pos(
          (
            p.c.productElement(perm._1).asInstanceOf[Int] * sign._1,
            p.c.productElement(perm._2).asInstanceOf[Int] * sign._2,
            p.c.productElement(perm._3).asInstanceOf[Int] * sign._3
          )
        )
    }
  }
}

// List("A", "B", "X", "C", "X", "D", "E", "F")
// List(List("A", "B"), List("C"), List("D", "E", "F"))
