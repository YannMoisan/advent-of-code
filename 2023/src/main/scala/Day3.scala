import scala.collection.mutable

object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val arr = input.toArray
    var sum = 0

    (0 until arr.length).foreach { i =>
      var number        = 0
      var ongoingNumber = false
      var detected      = false
      (0 until arr.head.length).foreach { j =>
        val c = arr(i)(j)
        if (c.isDigit) {
          number = number * 10 + c.asDigit
          ongoingNumber = true
          // look at neighbors
          for {
            ii <- (i - 1) to (i + 1)
            jj <- (j - 1) to (j + 1)
          } if (ii != i || jj != j) {
            if (ii >= 0 && ii < arr.length) {
              if (jj >= 0 && jj < arr.head.length) {
                if (!arr(ii)(jj).isDigit && arr(ii)(jj) != '.')
                  detected = true
              }
            }
          }
        } else {
          if (ongoingNumber) {
            if (detected)
              sum += number
            number = 0
          }
          ongoingNumber = false
          detected = false
        }
        // handle end of line
        if (j == arr.head.length - 1 && ongoingNumber && detected) {
          sum += number
        }
      }
    }
    sum
  }

  override def part2(input: Iterator[String]): Int = {
    val arr = input.toArray
    val m   = mutable.Map[(Int, Int), mutable.ListBuffer[Int]]()

    (0 until arr.length).foreach { i =>
      var number        = 0
      var ongoingNumber = false
      var detected      = mutable.Set[(Int, Int)]()
      (0 until arr.head.length).foreach { j =>
        val c = arr(i)(j)
        if (c.isDigit) {
          number = number * 10 + c.asDigit
          ongoingNumber = true
          // look at neighbors
          for {
            ii <- (i - 1) to (i + 1)
            jj <- (j - 1) to (j + 1)
          } if (ii != i || jj != j) {
            if (ii >= 0 && ii < arr.length) {
              if (jj >= 0 && jj < arr.head.length) {
                if (arr(ii)(jj) == '*')
                  detected.addOne((ii, jj))
              }
            }
          }
        } else {
          if (ongoingNumber) {
            detected.foreach { pos =>
              if (!m.isDefinedAt(pos)) {
                m(pos) = mutable.ListBuffer[Int]()
              }
              m(pos).addOne(number)
            }
            number = 0
          }
          ongoingNumber = false
          detected = mutable.Set[(Int, Int)]()
        }
        // handle end of line
        if (j == arr.head.length - 1 && ongoingNumber) {
          detected.foreach { pos =>
            if (!m.isDefinedAt(pos)) {
              m(pos) = mutable.ListBuffer[Int]()
            }
            m(pos).addOne(number)
          }
        }
      }
    }

    m.collect { case (_, v) if v.size == 2 => v.product }.sum

  }
}
