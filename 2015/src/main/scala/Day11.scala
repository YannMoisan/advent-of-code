// label: String, base26
@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day11 extends SinglePuzzle[String, String] {
  private val alphabetToBase26Mapping = ('a' to 'z').zip(('0' to '9') ++ ('a' to 'p')).toMap
  private val base26ToAlphabetMapping =
    ('a' to 'z').zip(('0' to '9') ++ ('a' to 'p')).toMap.map { case (k, v) => v -> k }
  private val allStraights: Seq[String] = ('a' to 'x').map { ch =>
    val ch2 = (ch + 1).toChar
    val ch3 = (ch + 2).toChar
    Seq(ch, ch2, ch3).mkString
  }
  private val allDouble: Seq[String] = ('a' to 'z').map(ch => Seq(ch, ch).mkString)

  override def part1(input: String): String =
    Iterator
      .iterate(input)(next).find(s =>
        includeIncreasingStraight(s) && notContainsIOL(s) && doubleLetters(s)
      ).get

  override def part2(input: String): String =
    Iterator
      .iterate("vzbxxyzz")(next).drop(1).find(s =>
        includeIncreasingStraight(s) && notContainsIOL(s) && doubleLetters(s)
      ).get

  def alphabetToBase26(s: String): String =
    s.map(alphabetToBase26Mapping)

  def base26ToAlphabet(s: String): String =
    s.map(base26ToAlphabetMapping)

  def next(s: String): String =
    base26ToAlphabet((BigInt(alphabetToBase26(s), 26) + 1).toString(26))

  def includeIncreasingStraight(s: String): Boolean =
    allStraights.exists(straight => s.contains(straight))

  def notContainsIOL(s: String): Boolean =
    !s.contains('i') && !s.contains('o') && !s.contains('l')

  def doubleLetters(s: String): Boolean =
    allDouble.count(s.contains) >= 2
}

// Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
// Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
// Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
