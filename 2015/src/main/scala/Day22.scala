import com.yannmoisan.util.graph.BFS

object Day22 extends SinglePuzzle[Int, Int] {
  case class State(
      mana: Int,
      hp: Int,
      armor: Int,
      countdown: Map[Spell, Int],
      bossHp: Int,
      bossDamage: Int,
      playerTurn: Boolean,
      manaUsed: Int
  )

  sealed abstract class Spell {
    def instant(s: State): State
    def delayed(s: State): State = s
  }
  case object MagicMissile extends Spell {
    override def instant(s: State): State = s.copy(
      mana = s.mana - 53,
      bossHp = s.bossHp - 4
    )
  }

  case object Drain extends Spell {
    override def instant(s: State): State = s.copy(
      mana = s.mana - 73,
      bossHp = s.bossHp - 2,
      hp = s.hp + 2
    )
  }

  case object Shield extends Spell {
    override def instant(s: State): State = s.copy(
      mana = s.mana - 113,
      countdown = s.countdown + ((Shield, 6))
    )
  }
  case object Poison extends Spell {
    override def instant(s: State): State = s.copy(
      mana = s.mana - 173,
      countdown = s.countdown + ((Poison, 6))
    )

    override def delayed(s: State): State = s.copy(
      bossHp = s.bossHp - 3
    )
  }
  case object Recharge extends Spell {
    override def instant(s: State): State = s.copy(
      mana = s.mana - 229,
      countdown = s.countdown + ((Recharge, 5))
    )

    override def delayed(s: State): State = s.copy(
      mana = s.mana + 101
    )
  }

  val spells = List(MagicMissile, Drain, Shield, Poison, Recharge)

  override def part1(input: String): Int = {
    val init = State(500, 50, 0, Map.empty, 71, 10, true, 0)
    val it   = BFS.breadth_first_traverse(init, children(false))
    it.filter(s => s._1.bossHp < 0).minBy(_._1.manaUsed)._1.manaUsed
  }

  override def part2(input: String): Int = {
    val init = State(500, 50, 0, Map.empty, 71, 10, true, 0)
    val it   = BFS.breadth_first_traverse(init, children(true))
    it.filter(s => s._1.bossHp <= 0).minBy(_._1.manaUsed)._1.manaUsed
  }

  def children(part2: Boolean)(s: State): Seq[State] =
    if (s.bossHp <= 0) Seq.empty
    else {
      if (s.playerTurn) {
        spells
          .map { spell =>
            val s15 = if (part2) s.copy(hp = s.hp - 1) else s
            val s2  = s.countdown.foldLeft(s15) { case (acc, (spell, _)) => spell.delayed(acc) }
            val s3  = decreaseCountdown(s2)
            val s4  = spell.instant(s3)
            val s5  = s4.copy(manaUsed = s.manaUsed + s3.mana - s4.mana)
            s5.copy(playerTurn = false)
          }.filter(s => s.mana >= 0 && s.hp >= 0)
      } else {
        val s2 = s.copy(
          hp = if (part2) -1 + s.hp else s.hp,
          playerTurn = true
        )
        val s3 = s2.countdown.foldLeft(s2) { case (acc, (spell, _)) => spell.delayed(acc) }
        val s4 = decreaseCountdown(s3)
        val s5 =
          if (s4.bossHp <= 0) s4
          else s4.copy(hp = s.hp - s.bossDamage + (if (s.countdown.contains(Shield)) 7 else 0))
        Seq(s5)
      }
    }

  private def decreaseCountdown(s: State): State = {
    val m  = s.countdown.view.mapValues(_ - 1).toMap
    val m2 = m.filter { case (_, v) => v > 0 }
    s.copy(countdown = m2)
  }
}
