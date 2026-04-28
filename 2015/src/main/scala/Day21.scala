object Day21 extends MultiPuzzle[Int, Int] {
  case class Item(cost: Int, damage: Int, armor: Int)

  case class Items(weapon: Item, armor: Option[Item], rings1: Option[Item], rings2: Option[Item]) {
    val cost = weapon.cost + armor.fold(0)(_.cost) + rings1.fold(0)(_.cost) + rings2.fold(0)(_.cost)
    val damage =
      weapon.damage + armor.fold(0)(_.damage) + rings1.fold(0)(_.damage) + rings2.fold(0)(_.damage)
    val armor0 =
      weapon.armor + armor.fold(0)(_.armor) + rings1.fold(0)(_.armor) + rings2.fold(0)(_.armor)
  }

  val weapons = List(
    Item(8, 4, 0),
    Item(10, 5, 0),
    Item(25, 6, 0),
    Item(40, 7, 0),
    Item(74, 8, 0)
  )

  val armors = List(
    Item(13, 0, 1),
    Item(31, 0, 2),
    Item(53, 0, 3),
    Item(75, 0, 4),
    Item(102, 0, 5)
  )

  val rings = List(
    Item(25, 1, 0),
    Item(50, 2, 0),
    Item(100, 3, 0),
    Item(20, 0, 1),
    Item(40, 0, 2),
    Item(80, 0, 3)
  )

  private def generate(): List[Items] =
    for {
      weapon <- weapons
      // TODO remove asInstanceOf
      armor <- None.asInstanceOf[Option[Item]] :: armors.map(Some(_))

      // TODO use case and a solution for non exhaustive match
      ring <- (None, None) :: (rings.map(ring => (Some(ring), None)) ++ rings.combinations(2).map {
        l =>
          val List(a, b) = l
          (Some(a), Some(b))
      })
    } yield Items(weapon, armor, ring._1, ring._2)

  case class Player(hp: Int, damage: Int, armor: Int)

  override def part1(input: Iterator[String]): Int = {
    val candidates = generate()
    candidates
      .filter(items => isWinner(Player(100, items.damage, items.armor0))).minBy(_.cost).cost
  }

  override def part2(input: Iterator[String]): Int = {
    val candidates = generate()
    candidates
      .filter(items => !isWinner(Player(100, items.damage, items.armor0))).maxBy(_.cost).cost
  }

  private def isWinner(p: Player): Boolean = {
    var current = 0
    var player  = p
    var boss    = Player(103, 9, 2) // from my input

    while (player.hp > 0 && boss.hp > 0)
      // Player turn
      if (current == 0) {
        boss = Player(boss.hp - math.max(1, player.damage - boss.armor), boss.damage, boss.armor)
        current = 1
        // Boss turn
      } else {
        player =
          Player(player.hp - math.max(1, boss.damage - player.armor), player.damage, player.armor)
        current = 0
      }
    player.hp > 0
  }
}
