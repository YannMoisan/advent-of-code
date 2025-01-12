object Day17 extends MultiPuzzle[Int, Int] {
  import Day17.Instruction._

  case class Registers(a: Int, b: Int, c: Int, ptr: Int, out: String, expected: String)
  case class Program(instructions: Array[Int]) {
    def execute(initReg: Registers): Registers = {
      var reg = initReg
      // read current instruction
      while (reg.ptr < instructions.length && reg.expected.startsWith(reg.out)) {
        val inst = instructions(reg.ptr) match {
          case 0 => adv(instructions(reg.ptr + 1))
          case 1 => bxl(instructions(reg.ptr + 1))
          case 2 => bst(instructions(reg.ptr + 1))
          case 3 => jnz(instructions(reg.ptr + 1))
          case 4 => bxc(instructions(reg.ptr + 1))
          case 5 => out(instructions(reg.ptr + 1))
          case 6 => bdv(instructions(reg.ptr + 1))
          case 7 => cdv(instructions(reg.ptr + 1))
        }
        reg = inst.execute(reg)
      }
      reg
    }
  }

  sealed trait Instruction {
    def execute(registers: Registers): Registers
  }
  object Instruction {
    case class adv(combo: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(
          a = (registers.a / math.pow(2, value(combo, registers).toDouble)).toInt,
          ptr = registers.ptr + 2
        )
    }
    case class bxl(literal: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(b = registers.b ^ literal, ptr = registers.ptr + 2)
    }
    case class bst(combo: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(b = value(combo, registers) % 8, ptr = registers.ptr + 2)
    }
    case class jnz(literal: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        if (registers.a == 0) registers.copy(ptr = registers.ptr + 2)
        else
          registers.copy(ptr = literal)
    }
    case class bxc(literal: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(b = registers.b ^ registers.c, ptr = registers.ptr + 2)
    }
    case class out(combo: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
//        print(value(combo, registers) % 8)
//        print(",")
        registers.copy(
          out = s"${registers.out}${value(combo, registers) % 8},",
          ptr = registers.ptr + 2
        )
    }

    case class bdv(combo: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(
          b = (registers.a / math.pow(2, value(combo, registers).toDouble)).toInt,
          ptr = registers.ptr + 2
        )
    }

    case class cdv(combo: Int) extends Instruction {
      override def execute(registers: Registers): Registers =
        registers.copy(
          c = (registers.a / math.pow(2, value(combo, registers).toDouble)).toInt,
          ptr = registers.ptr + 2
        )
    }

  }

  def value(i: Int, reg: Registers): Int =
    i match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => reg.a
      case 5 => reg.b
      case 6 => reg.c
      case 7 => throw new IllegalStateException()
    }

  override def part1(input: Iterator[String]): Int = {
    val s  = "2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0"
    val s2 = "2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0,"

    (0 to Int.MaxValue).foreach { i =>
      val reg = Program(s.split(",").map(_.toInt)).execute(Registers(i, 0, 0, 0, "", s2))
      if (reg.out == s2)
        println(s"---$i---")
    }

    42
  }

  override def part2(input: Iterator[String]): Int = ???
}
