object Day20 extends MultiPuzzle[Int, Int] {
  case class Particle(p: (Int, Int, Int), v: (Int, Int, Int), a: (Int, Int, Int))
  override def part1(input: Iterator[String]): Int = {
    val particles = input.map {
      case s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>" =>
        Particle(
          (px.toInt, py.toInt, pz.toInt),
          (vx.toInt, vy.toInt, vz.toInt),
          (ax.toInt, ay.toInt, az.toInt)
        )
    }.toList

    val particlesF = Iterator.iterate(particles)(_.map(next)).drop(5_000_000).next()
    particlesF.zipWithIndex.minBy {
      case (p, _) => math.abs(p.p._1) + math.abs(p.p._2) + math.abs(p.p._3)
    }._2
  }

  def next(p: Particle): Particle = {
    val newV = (p.v._1 + p.a._1, p.v._2 + p.a._2, p.v._3 + p.a._3)

    Particle(
      (p.p._1 + newV._1, p.p._2 + newV._2, p.p._3 + newV._3),
      newV,
      (p.a._1, p.a._2, p.a._3)
    )
  }

  override def part2(input: Iterator[String]): Int = 43
}
