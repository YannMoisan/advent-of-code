object Day9 extends MultiPuzzle[Int, Int] {
  def nextLastValue(l: List[Int]): Int = {
    val seqs = allSequences(l)
    (seqs.length - 2 to 0 by -1).foldLeft(0) { case (acc, index) => seqs(index).last + acc }
  }

  def nextFirstValue(l: List[Int]): Int = {
    val seqs = allSequences(l)
    (seqs.length - 2 to 0 by -1).foldLeft(0) { case (acc, index) => seqs(index).head - acc }
  }

  def allSequences(l: List[Int]): List[List[Int]] = {
    var sequences = List(l)
    while (!sequences.last.forall(_ == 0)) {
      sequences = sequences :+ nextSequence(sequences.last)
    }
    sequences
  }

  def nextSequence(l: List[Int]): List[Int] =
    l.sliding(2).collect { case List(i, j) => j - i }.toList

  override def part1(input: Iterator[String]): Int =
    input
      .map(line => line.split(' ').map(_.toInt).toList)
      .map(nextLastValue)
      .sum

  override def part2(input: Iterator[String]): Int =
    input
      .map(line => line.split(' ').map(_.toInt).toList)
      .map(nextFirstValue)
      .sum
}
