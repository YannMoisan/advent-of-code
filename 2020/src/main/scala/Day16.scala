object Day16 extends MultiPuzzle[Int, Long] {
  case class MyRange(min: Int, max: Int) {
    def contains(v: Int): Boolean = v >= min && v <= max
  }
  case class Rule(field: String, range1: MyRange, range2: MyRange) {
    def check(v: Int): Boolean = range1.contains(v) || range2.contains(v)
  }

  override def part1(input: Iterator[String]): Int = {
    val (rules, _, tickets) = parse(input)
    tickets.map(ticket => ticket.filter(!isValid(rules)(_)).sum).sum
  }

  override def part2(input: Iterator[String]): Long = {
    val (rules, myTicket, tickets) = parse(input)

    val validTickets = tickets.filter(ticket => ticket.forall(isValid(rules)))

    val possibleRulesByField: Seq[Set[Rule]] = validTickets.head.indices.map(i =>
      validTickets
        .map(ticket => rules.filter(_.check(ticket(i))).toSet)
        .reduce(_.intersect(_))
    )

    Iterator
      .unfold(possibleRulesByField)(resolve)
      .collect { case (fieldId, rule) if rule.field.startsWith("departure") => fieldId }
      .map(myTicket(_).toLong)
      .product
  }

  private def isValid(rules: Array[Rule])(v: Int): Boolean =
    rules.exists(_.check(v))

  private def resolve(possibleRules: Seq[Set[Rule]]): Option[((Int, Rule), Seq[Set[Rule]])] =
    possibleRules.indices
      .find(fieldId => possibleRules(fieldId).size == 1)
      .map { fieldId =>
        val rule         = possibleRules(fieldId).head
        val updatedRules = possibleRules.map(_ - rule)
        ((fieldId, rule), updatedRules)
      }

  private def parse(input: Iterator[String]): (Array[Rule], Array[Int], List[Array[Int]]) = {
    val inputList = input.toList

    val rules = inputList.collect {
      case s"$field: $min1-$max1 or $min2-$max2" =>
        Rule(field, MyRange(min1.toInt, max1.toInt), MyRange(min2.toInt, max2.toInt))
    }.toArray

    val tickets = inputList.collect {
      case line if !line.isEmpty && line(0).isDigit => line.split(",").map(_.toInt)
    }

    (rules, tickets.head, tickets.tail)
  }
}
