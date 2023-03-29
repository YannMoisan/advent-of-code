import scala.math.{max, min}

// TODO : lens, state monad
object Day10 extends MultiPuzzle[String, Int] {

  case class Bot(id: String, low: Option[Int], high: Option[Int]) {
    def addChip(chipId: Int): Bot = this match {
      case Bot(_, None, None) => this.copy(low = Some(chipId))
      case Bot(_, Some(curChipId), None) =>
        this.copy(low = Some(min(chipId, curChipId)), high = Some(max(chipId, curChipId)))
      case _ => throw new IllegalStateException()

    }

    def removeChips(): Bot = this.copy(low = None, high = None)
  }

  case class Output(id: String, chipId: Int)

  case class State(
      bots: Map[String, Bot],
      outputs: Map[String, Output],
      is: Seq[Instruction],
      cond: Option[String]
  ) {
    def addRecipient(recipient: Recipient, chipId: Int) = recipient match {
      case BotRecipient(id)    => addChip(id, chipId)
      case OutputRecipient(id) => addOutput(id, chipId)
    }

    private def addChip(botId: String, chipId: Int) = {
      val newBot = bots.get(botId) match {
        case None      => Bot(botId, Some(chipId), None)
        case Some(bot) => bot.addChip(chipId)
      }
      this.copy(bots = bots.updated(newBot.id, newBot))
    }

    private def addOutput(outputId: String, chipId: Int) =
      this.copy(outputs = outputs.updated(outputId, Output(outputId, chipId)))
  }

  trait Recipient

  case class BotRecipient(id: String) extends Recipient

  case class OutputRecipient(id: String) extends Recipient

  object Recipient {
    def apply(typ: String, id: String): Recipient =
      if (typ == "bot") BotRecipient(id) else OutputRecipient(id)
  }

  trait Instruction

  case class GiveFromInput(chipId: Int, recipient: BotRecipient) extends Instruction

  case class GiveFromBot(botId: String, lowRecipient: Recipient, highRecipient: Recipient)
      extends Instruction

  def run[A](s: Seq[A => A], init: A): A = s.foldLeft(init) { case (acc, f) => f(acc) }

  def processNextInstruction(acc: State): State = {
    val next = acc.is.find(i => isValid(acc, i)).get

    val updates: Seq[State => State] = next match {
      case GiveFromInput(chipId, recipient) => Seq(_.addRecipient(recipient, chipId))
      case GiveFromBot(botId, lowRecipient, highRecipient) =>
        Seq(
          s => s.addRecipient(lowRecipient, s.bots(botId).low.get),
          s => s.addRecipient(highRecipient, s.bots(botId).high.get),
          s =>
            s.copy(cond =
              if (s.bots(botId).low.get == 17 && s.bots(botId).high.get == 61) Some(botId) else None
            ),
          s => s.copy(bots = s.bots.updated(botId, s.bots(botId).removeChips()))
        )
    }

    val removeInstr: State => State = _.copy(is = acc.is.filterNot(_ == next).toList)
    run(updates :+ removeInstr, acc)
  }

  def isValid(s: State, instr: Instruction) = instr match {
    case GiveFromInput(_, _) => true
    case GiveFromBot(botId, _, _) => {
      // fromBot should have 2 chips
      s.bots.get(botId) match {
        case Some(Bot(_, Some(_), Some(_))) => true
        case _                              => false
      }
    }
  }

  val giveFromInput = """value (\d+) goes to bot (\d+)""".r
  val giveFromBot   = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

  def parse: PartialFunction[String, Instruction] = {
    case giveFromInput(chipId, botId) =>
      GiveFromInput(chipId.toInt, BotRecipient(botId))
    case giveFromBot(botId, lowType, lowId, highType, highId) =>
      GiveFromBot(botId, Recipient(lowType, lowId), Recipient(highType, highId))
  }

  override def part1(lines: Iterator[String]): String = {
    val instructions = lines.collect {
      parse
    }.toList

    val init = State(Map[String, Bot](), Map[String, Output](), instructions, None)

    Iterator
      .iterate(init)(processNextInstruction)
      .find(_.cond.isDefined).get
      .cond.get
  }

  override def part2(lines: Iterator[String]): Int = {
    val instructions: Seq[Instruction] = lines.collect {
      parse
    }.toList

    val init = State(Map[String, Bot](), Map[String, Output](), instructions, None)

    val finalState = Iterator
      .iterate(init)(processNextInstruction)
      .find(_.is.isEmpty).get

    val o = finalState.outputs
    o("0").chipId * o("1").chipId * o("2").chipId
  }

}
