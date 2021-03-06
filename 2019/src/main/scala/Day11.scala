import Day11.ParameterMode.{Immediate, Position, Relative}

import scala.collection.mutable

object Day11 extends SinglePuzzle[Int, Int] {
  override def part1: String => Int = { line =>
    val program = line.split(",").map(_.toLong)

    computeOutput(
      Memory(program, mutable.Map.empty[Long, Long])
    )
  }

  override def part2: String => Int = { line =>
    val program = line.split(",").map(_.toLong)

    computeOutput(Memory(program, mutable.Map.empty[Long, Long]))
  }

  private def computeOutput(program: Memory): Int = {
    var pointer        = 0L
    var inputPtr       = 0
    var continue       = true
    val outputs        = mutable.Buffer[Long]()
    var relativeBase   = 0L
    var posx           = 0
    var posy           = 0
    var dir: Direction = Direction.Up
    var outputCount    = 0
    val panel          = mutable.Map.empty[(Int, Int), Int]
    panel.put((0, 0), 1)
    while (continue) {
      val instr = Instruction.from(program, pointer, relativeBase)
      //println(instr)
      instr.opcode match {
        case 1 =>
          instr.write(3, instr.read(1) + instr.read(2))
          pointer += 4
        case 2 =>
          instr.write(3, instr.read(1) * instr.read(2))
          pointer += 4
        case 3 =>
          val input = panel.getOrElse((posx, posy), 0)

          println("input=" + input)
          instr.write(1, input.toLong)
          inputPtr += 1
          pointer += 2
        case 4 =>
          println("output=" + instr.read(1))
          if (outputCount % 2 == 0) {
            // paint
            panel.put((posx, posy), instr.read(1).toInt)
          } else {
            // move
            dir = Direction.next(dir, instr.read(1) != 0L)
            posx = posx + dir.dx
            posy = posy + dir.dy
          }
          outputCount += 1
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
        case 9 =>
          relativeBase += instr.read(1)
          //println("modify=" + relativeBase)
          pointer += 2
        case 99 => continue = false
      }
    }
    //println(panel.toMap.keys.maxBy())
    println("max x:" + panel.maxBy(_._1._1))
    println("min x:" + panel.minBy(_._1._1))
    println("max y:" + panel.maxBy(_._1._2))
    println("min y:" + panel.minBy(_._1._2))

    println((0 to 42).map(x => if (panel.getOrElse((x, 0), 0) == 1) '*' else ' ').mkString)
    println((0 to 42).map(x => if (panel.getOrElse((x, 1), 0) == 1) '*' else ' ').mkString)
    println((0 to 42).map(x => if (panel.getOrElse((x, 2), 0) == 1) '*' else ' ').mkString)
    println((0 to 42).map(x => if (panel.getOrElse((x, 3), 0) == 1) '*' else ' ').mkString)
    println((0 to 42).map(x => if (panel.getOrElse((x, 4), 0) == 1) '*' else ' ').mkString)
    println((0 to 42).map(x => if (panel.getOrElse((x, 5), 0) == 1) '*' else ' ').mkString)

    //  .maxBy{x => x._1}
    panel.size
  }

//  private def computeOutput2(
//      statee: Statee
//  ): Int = {
//    var continue = true
//    val outputs  = mutable.Buffer[Int]()
//    while (continue) {
//      val instr = Instruction.from(statee.p, statee.ptr)
//      instr.opcode match {
//        case 1 =>
//          instr.write(3, instr.read(1) + instr.read(2))
//          statee.ptr += 4
//        case 2 =>
//          instr.write(3, instr.read(1) * instr.read(2))
//          statee.ptr += 4
//        case 3 =>
//          instr.write(1, statee.inputs(statee.iptr))
//          statee.iptr += 1
//          statee.ptr += 2
//        case 4 =>
//          println(s"output=${instr.read(1)}")
//          outputs.addOne(instr.read(1))
//          statee.ptr += 2
//          continue = false
//        case 5 =>
//          if (instr.read(1) != 0)
//            statee.ptr = instr.read(2)
//          else
//            statee.ptr += 3
//        case 6 =>
//          if (instr.read(1) == 0)
//            statee.ptr = instr.read(2)
//          else
//            statee.ptr += 3
//        case 7 =>
//          if (instr.read(1) < instr.read(2))
//            instr.write(3, 1)
//          else
//            instr.write(3, 0)
//          statee.ptr += 4
//        case 8 =>
//          if (instr.read(1) == instr.read(2))
//            instr.write(3, 1)
//          else
//            instr.write(3, 0)
//          statee.ptr += 4
//        case 99 => continue = false
//      }
//    }
//    if (outputs.isEmpty) -666 else outputs.last
//  }
  sealed trait ParameterMode extends Product with Serializable

  object ParameterMode {

    case object Immediate extends ParameterMode

    case object Position extends ParameterMode

    case object Relative extends ParameterMode

    def from(i: Char): ParameterMode = i match {
      case '0' => Position
      case '1' => Immediate
      case '2' => Relative
    }
  }

  case class Memory(p: Array[Long], extraMemory: mutable.Map[Long, Long]) {
    def read(adr: Long): Long = {
      if (adr < p.length) p(adr.toInt) else extraMemory.getOrElse(adr, 0L)
    }

    def write(adr: Long, value: Long) =
      if (adr < p.length) p(adr.toInt) = value else extraMemory.put(adr, value)

  }

  case class Instruction(
      program: Memory,
      pointer: Long,
      opcode: Int,
      modes: Array[ParameterMode],
      relativeBase: Long
  ) {
    // parameter index starts at 1
    def read(parameterIndex: Int): Long = {
      val address = mode(parameterIndex - 1) match {
        case Immediate => pointer + parameterIndex
        case Position  => program.read(pointer + parameterIndex)
        case Relative  => relativeBase + program.read(pointer + parameterIndex)
      }
      program.read(address)
    }
    def write(parameterIndex: Int, value: Long): Unit = {
      val address = mode(parameterIndex - 1) match {
        case Immediate => pointer + parameterIndex
        case Position  => program.read(pointer + parameterIndex)
        case Relative  => relativeBase + program.read(pointer + parameterIndex)
      }
      val _ = program.write(address, value)
    }

    private def mode(paramIndex: Int): ParameterMode =
      if (paramIndex >= modes.length) Position
      else modes(modes.length - paramIndex - 1)
  }

  case class Program(codes: Array[Long], pointer: Long)

  object Instruction {
    def from(program: Memory, pointer: Long, relativeBase: Long): Instruction = {
      val rawInstruction = program.read(pointer)
      val opcode         = rawInstruction.toString.takeRight(2).toInt
      val modes          = rawInstruction.toString.dropRight(2).toArray
      Instruction(program, pointer, opcode, modes.map(ParameterMode.from), relativeBase)
    }
  }

  sealed abstract class Direction(val dx: Int, val dy: Int) extends Product with Serializable

  object Direction {

    case object Up extends Direction(0, -1)

    case object Down extends Direction(0, 1)

    case object Left extends Direction(-1, 0)

    case object Right extends Direction(1, 0)

    val all: Array[Direction] = Array(Up, Right, Down, Left)

    def next(d: Direction, clockWise: Boolean): Direction = {
      val nextIndex = all.indexOf(d) + (if (clockWise) 1 else -1) match {
        case -1 => 3
        case 4  => 0
        case i  => i
      }
      all(nextIndex)
    }
  }
}
