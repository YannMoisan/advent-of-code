import OpCode.opcodes

object Day19 extends MultiPuzzle[Int, Int] {

  private val registerCount = 6

  case class Instruction(opCode: OpCode, in1: Int, in2: Int, out: Int)

  override def part1(input: Iterator[String]): Int = {
//    val ip = 2
    opcodes.foreach(op => println(op.getClass.getName))
    val instructions: Array[Instruction] = input
      .drop(1).map { line =>
        val Array(strOpCode, in1, in2, out) = line.split(" ")
        Instruction(
          opcodes.find(_.getClass.getName.contains(strOpCode)).get,
          in1.toInt,
          in2.toInt,
          out.toInt
        )
      }.toArray
    val start = Vector.fill(registerCount)(0)
    Iterator.iterate(start)(next(instructions)).find(_.apply(2) >= instructions.size).get.apply(0)
  }

  override def part2(input: Iterator[String]): Int = {
    //    val ip = 2
    opcodes.foreach(op => println(op.getClass.getName))
    val instructions: Array[Instruction] = input
      .drop(1).map { line =>
        val Array(strOpCode, in1, in2, out) = line.split(" ")
        Instruction(
          opcodes.find(_.getClass.getName.contains(strOpCode)).get,
          in1.toInt,
          in2.toInt,
          out.toInt
        )
      }.toArray
    val start = Vector(1, 0, 0, 0, 0, 0)
    //.fill(registerCount)(0)
    Iterator.iterate(start)(next(instructions)).find(_.apply(2) >= instructions.size).get.apply(0)
  }

  private def next(instructions: Array[Instruction])(regs: Vector[Int]): Vector[Int] = {
    val i       = instructions(regs(2))
    val newRegs = i.opCode.execute(i.in1, i.in2, i.out)(regs)
    newRegs.updated(2, newRegs(2) + 1)
  }

}
