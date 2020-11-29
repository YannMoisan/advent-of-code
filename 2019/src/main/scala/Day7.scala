import scala.collection.mutable

object Day7 extends SinglePuzzle[Int, Int] {
  override def part1: String => Int = { line =>
    val program = line.split(",").map(_.toInt)

    val combs = (0 to 4).permutations.toArray

    combs.map { comb =>
      val o0 = computeOutput(program.clone(), Array(comb(0), 0))
      val o1 = computeOutput(program.clone(), Array(comb(1), o0))
      val o2 = computeOutput(program.clone(), Array(comb(2), o1))
      val o3 = computeOutput(program.clone(), Array(comb(3), o2))
      computeOutput(program.clone(), Array(comb(4), o3))
    }.max

  }

  class State(var p: Array[Int], var ptr: Int, var iptr: Int, var inputs: mutable.Buffer[Int])

  override def part2: String => Int = { line =>
    val program = line.split(",").map(_.toInt)

    val combs: Array[IndexedSeq[Int]] = (5 to 9).permutations.toArray

    combs.map { comb =>
      val initStates = Array(
        new State(program.clone(), 0, 0, mutable.Buffer(comb(0), 0)),
        new State(program.clone(), 0, 0, mutable.Buffer(comb(1))),
        new State(program.clone(), 0, 0, mutable.Buffer(comb(2))),
        new State(program.clone(), 0, 0, mutable.Buffer(comb(3))),
        new State(program.clone(), 0, 0, mutable.Buffer(comb(4)))
      )

      var continue   = true
      var i          = 0
      var lastOutput = -1
      while (continue) {
        println(s"amp=$i, ptr=${initStates(i).ptr}")
        val o =
          computeOutput2(initStates(i))
        if (o != -666) lastOutput = o

        if (i == 4 && initStates(i).p(initStates(i).ptr) == 99)
          continue = false
        else {
          i = (i + 1) % 5
          initStates(i).inputs.addOne(o)
        }
      }
      //println("input4=" + initStates(4).inputs.mkString(","))
      lastOutput
    //initStates(0).inputs.last
    }.max
  }

  private def computeOutput(program: Array[Int], inputs: Array[Int]): Int = {
    var pointer  = 0
    var inputPtr = 0
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
          instr.write(1, inputs(inputPtr))
          inputPtr += 1
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

  private def computeOutput2(
      statee: State
  ): Int = {
    var continue = true
    val outputs  = mutable.Buffer[Int]()
    while (continue) {
      val instr = Instruction.from(statee.p, statee.ptr)
      instr.opcode match {
        case 1 =>
          instr.write(3, instr.read(1) + instr.read(2))
          statee.ptr += 4
        case 2 =>
          instr.write(3, instr.read(1) * instr.read(2))
          statee.ptr += 4
        case 3 =>
          instr.write(1, statee.inputs(statee.iptr))
          statee.iptr += 1
          statee.ptr += 2
        case 4 =>
          println(s"output=${instr.read(1)}")
          outputs.addOne(instr.read(1))
          statee.ptr += 2
          continue = false
        case 5 =>
          if (instr.read(1) != 0)
            statee.ptr = instr.read(2)
          else
            statee.ptr += 3
        case 6 =>
          if (instr.read(1) == 0)
            statee.ptr = instr.read(2)
          else
            statee.ptr += 3
        case 7 =>
          if (instr.read(1) < instr.read(2))
            instr.write(3, 1)
          else
            instr.write(3, 0)
          statee.ptr += 4
        case 8 =>
          if (instr.read(1) == instr.read(2))
            instr.write(3, 1)
          else
            instr.write(3, 0)
          statee.ptr += 4
        case 99 => continue = false
      }
    }
    if (outputs.isEmpty) -666 else outputs.last
  }
}
