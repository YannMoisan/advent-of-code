import com.yannmoisan.util.grid.Dijkstra.dijkstra
import com.yannmoisan.util.grid.Grid1D

object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]) =
    dijkstra(Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit))))

  override def part2(input: Iterator[String]) =
    dijkstra(Grid1D(fiveTimesLarger(input.toArray.map(_.toCharArray.map(_.asDigit)))))

  def fiveTimesLarger(input: Array[Array[Int]]): Array[Array[Int]] =
    Array
      .tabulate(input.head.length * 5, input.length * 5) { (x, y) =>
        val dx    = x / input.head.length
        val dy    = y / input.length
        val value = input(y % input.head.length)(x % input.length) + dx + dy
        if (value > 9) value - 9 else value
      }

}
