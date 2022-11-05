import ParameterMode.{Immediate, Position}

import scala.collection.mutable

object Day5 extends SinglePuzzle[Int, Int] {
  override def part1(line: String) : Int = {
    val program = line.split(",").map(_.toInt)
    computeOutput(program, 1)
  }

  override def part2(line: String) : Int = {
    val program = line.split(",").map(_.toInt)
    computeOutput(program, 5)
  }

  private def computeOutput(program: Array[Int], input: Int): Int = {
    var pointer  = 0
    var continue = true
    val outputs  = mutable.Buffer[Int]()
    while (continue) {
      val instr = Instruction.from(program, pointer)
      instr.opcode match {
        case 1 =>
          instr.write(3, instr.read(1) + instr.read(2))
          pointer += 4
        case 2 =>
          instr.write(3, instr.read(1) * instr.read(2))
          pointer += 4
        case 3 =>
          instr.write(1, input)
          pointer += 2
        case 4 =>
          outputs.addOne(instr.read(1))
          pointer += 2
        case 5 =>
          if (instr.read(1) != 0)
            pointer = instr.read(2)
          else
            pointer += 3
        case 6 =>
          if (instr.read(1) == 0)
            pointer = instr.read(2)
          else
            pointer += 3
        case 7 =>
          if (instr.read(1) < instr.read(2))
            instr.write(3, 1)
          else
            instr.write(3, 0)
          pointer += 4
        case 8 =>
          if (instr.read(1) == instr.read(2))
            instr.write(3, 1)
          else
            instr.write(3, 0)
          pointer += 4
        case 99 => continue = false
      }
    }
    outputs.last
  }
}

sealed trait ParameterMode extends Product with Serializable

object ParameterMode {
  case object Immediate extends ParameterMode
  case object Position  extends ParameterMode

  def from(i: Char): ParameterMode = if (i == '0') Position else Immediate
}

case class Instruction(
    program: Array[Int],
    pointer: Int,
    opcode: Int,
    modes: Array[ParameterMode]
) {
  // parameter index starts at 1
  def read(parameterIndex: Int): Int = {
    val address = mode(parameterIndex - 1) match {
      case Immediate => pointer + parameterIndex
      case Position  => program(pointer + parameterIndex)
    }
    program(address)
  }
  def write(parameterIndex: Int, value: Int): Unit =
    program(program(pointer + parameterIndex)) = value

  private def mode(paramIndex: Int): ParameterMode =
    if (paramIndex >= modes.length) Position
    else modes(modes.length - paramIndex - 1)
}

case class Program(codes: Array[Int], pointer: Int)

object Instruction {
  def from(program: Array[Int], pointer: Int): Instruction = {
    val rawInstruction = program(pointer)
    val opcode         = rawInstruction.toString.takeRight(2).toInt
    val modes          = rawInstruction.toString.dropRight(2).toArray
    Instruction(program, pointer, opcode, modes.map(ParameterMode.from))
  }
}
