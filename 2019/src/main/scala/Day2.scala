object Day2 extends SinglePuzzle[Int, Int] {
  override def part1(line: String): Int = {
    val program = line.split(",").map(_.toInt)
    computeOutput(program, 12, 2)
  }

  override def part2(line: String): Int = {
    val program = line.split(",").map(_.toInt)
    val outputs: Iterator[((Int, Int), Int)] = for {
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
    } yield {
      (noun, verb) -> computeOutput(program.clone(), noun, verb)
    }
    outputs.toMap
      .find(_._2 == 19690720)
      .map { case ((noun, verb), _) => 100 * noun + verb }
      .getOrElse(throw new IllegalStateException())
  }

  private def computeOutput(program: Array[Int], noun: Int, verb: Int) = {
    program(1) = noun
    program(2) = verb

    var pointer  = 0
    var continue = true
    while (continue) {
      val code = program(pointer)
      code match {
        case 1 =>
          program(program(pointer + 3)) = program(program(pointer + 1)) + program(
            program(pointer + 2)
          )
          pointer += 4
        case 2 =>
          program(program(pointer + 3)) = program(program(pointer + 1)) * program(
            program(pointer + 2)
          )
          pointer += 4
        case 99 => continue = false
      }
    }
    program(0)

  }
}
