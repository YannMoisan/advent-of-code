import scala.collection.immutable.Vector

sealed trait OpCode {
  def op(in1: Int, in2: Int)(regs: Vector[Int]): Int

  def execute(in1: Int, in2: Int, out: Int)(regs: Vector[Int]): Vector[Int] =
    regs.updated(out, op(in1, in2)(regs))
}

object OpCode {
  case object addr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) + regs(in2)
  }

  case object addi extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) + in2
  }

  case object mulr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) * regs(in2)
  }

  case object muli extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) * in2
  }

  case object banr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) & regs(in2)
  }

  case object bani extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) & in2
  }

  case object borr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) | regs(in2)
  }

  case object bori extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1) | in2
  }

  case object setr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = regs(in1)
  }

  case object seti extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = in1
  }

  case object gtir extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = (if (in1 > regs(in2)) 1 else 0)
  }

  case object gtri extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = (if (regs(in1) > in2) 1 else 0)
  }

  case object gtrr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int =
      (if (regs(in1) > regs(in2)) 1 else 0)
  }

  case object eqir extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = (if (in1 == regs(in2)) 1 else 0)
  }

  case object eqri extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int = (if (regs(in1) == in2) 1 else 0)
  }

  case object eqrr extends OpCode {
    override def op(in1: Int, in2: Int)(regs: Vector[Int]): Int =
      (if (regs(in1) == regs(in2)) 1 else 0)
  }

  val opcodes: Seq[OpCode] = Seq(
    addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr
  )
}
