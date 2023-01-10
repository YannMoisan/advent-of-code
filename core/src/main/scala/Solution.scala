import scala.collection.mutable
import scala.io.StdIn.readLine

// https://www.codingame.com/ide/puzzle/bulls-and-cows
// TODO Find permutation of (bull, bull, cow, n/a)

object Solution extends App {
  val n = readLine().toInt
  val guesses = List.fill(n) {
    val Array(guess, _bulls, _cows) = readLine() split " "
    Guess(guess, _bulls.toInt, _cows.toInt)
  }
  println(BruteForce.solve(guesses))
}

case class Guess(guess: String, bulls: Int, cows: Int)

trait BullsAndCows {
  def solve(guesses: List[Guess]) : String
}

object BruteForce extends BullsAndCows {
  override def solve(guesses: List[Guess]): String = {
    val candidates = Combinatorics.permutationsWithReplacement(('0' to '9').toList, 4).map(_.mkString)
    candidates.find { candidate =>
      guesses.forall { guess => evaluate(candidate, guess.guess) == ((guess.bulls, guess.cows)) }
    }.get
  }

  private def evaluate(sol: String, guess: String): (Int, Int) = {
    // Find bulls
    var bulls = 0
    var cows = 0
    // populate the state for the algo
    val state = mutable.Map[Char, Int]()
    sol.foreach { c => state.update(c, 1 + state.getOrElse(c, 0)) }

    (0 to 3).foreach { i =>
      if (sol(i) == guess(i)) {
        bulls += 1
        state.update(guess(i), state(guess(i)) - 1)
      }
    }

    (0 to 3).foreach { i =>
      if (sol(i) != guess(i)) { // not a bull, might be a cow
        if (state.get(guess(i)).exists(_ > 0)) {
          cows += 1
          state.update(guess(i), state(guess(i)) - 1)
        }
      }
    }
    (bulls, cows)
  }
}

object Solution2 extends App {
  val guess = "0123"

  val chars = ('0' to '9').toSet

  def insertAt[A](l: List[A], i: Int, e: A) : List[A] = {
    val (start, end) = l.splitAt(i)
    start ++ List(e) ++ end
  }

  def removeAt(s: String, i: Int) : String = {
    s.substring(0, i) + s.substring(i+1)
  }

  // Example
  // guess = ABCD
  // goodIndices = (0,1)
  // replacement = XY
  // => result = ABXY
  def combine(guess: String, goodIndices: Seq[Int], replacement: String) = {
    var j = 0
    (0 to 3).map { i =>
      if (goodIndices.contains(i))
        guess(i)
      else {
        val c = replacement(j)
        j += 1
        c
      }
    }.mkString
  }

  def generate(guess: String, bulls: Int, cows: Int) : Seq[String] = {
    if (bulls == 4 && cows == 0) {
      List(guess)
    }
    else
      if (bulls == 0 && cows == 0) {
        val excluded: Set[Char] = (0 to 3).map(guess(_)).toSet
        Combinatorics.permutationsWithReplacement(chars.diff(excluded).toList, 4).map(_.mkString)
      }
      //    else if (bulls == 1 && cows == 0) {
      //      (0 to 3).flatMap { i =>
      //        val excluded: Set[Char] = (0 to 3).filter(_ != i).map { guess(_) }.toSet
      //        val res = Combinatorics.permutationsWithReplacement(chars.diff(excluded).toList, 3)
      //        res.map { r => insertAt(r, i, guess(i)).mkString }
      //      }
      //    }
      else if ((bulls == 1 || bulls == 2 || bulls == 3 || bulls==4) && cows == 0) {
        val bullIndicesCandidates = Combinatorics.combinations(List(0, 1, 2, 3), bulls)
        println(s"bulls=$bulls, #bullIndicesCandidates=${bullIndicesCandidates.size}")
        bullIndicesCandidates.flatMap { positions =>
          val excluded: Set[Char] = chars.diff(List(0, 1, 2, 3).filter(x => !positions.contains(x)).map(guess(_)).toSet)
          val res = Combinatorics.permutationsWithReplacement(excluded.toList, 4 - bulls)
          res.map(r => combine(guess, positions, r.mkString))
        }
      }
      //      else if (bulls == 3 && cows == 0) {
      //         val combi = Combinatorics.combinations(List(0,1,2,3), 3)
      //          combi.flatMap { positions =>
      //            val excluded: Set[Char] = chars.diff(List(0,1,2,3).filter(x => !positions.contains(x)).map(guess(_)).toSet)
      //            val l = excluded.toList.map { c =>
      //              val m: Seq[Char] = (0 to 3)
      //                .map { i=> if (positions.contains(i)) guess(i) else c }
      //              m.mkString
      //            }
      //            l
      //        }
      //      }
      else {
        throw new UnsupportedOperationException()
      }
  }

  // All bulls
  // 1234 4 0
  println(List(
    generate("1234", 4, 0)
  ).reduce {
    _ intersect _
  }
  )

  // 2 - pairs of cows
  // 0473 2 2
  //7403 0 4

  // 3 - two pair of bulls
  // 9073 2 0
  // 1248 2 0
  // 1043 0 0
  println(List(
    generate("9073", 2, 0),
    generate("1248", 2, 0),
    generate("1043", 0, 0),
  ).reduce {
    _ intersect _
  }
  )

  // 4 - nothing but cows
  // 7878 0 4

  // 5 - Lone bulls
  // 0123 1 0
  // 4567 1 0
  // 8901 1 0
  // 8522 3 0
  // 8525 3 0
  println(List(
    generate("0123", 1, 0),
    generate("4567", 1, 0),
    generate("8901", 1, 0),
    generate("8522", 3, 0),
    generate("8525", 3, 0)
  ).reduce {
    _ intersect _
  }
  )

  // 6 - one for all
  // 0123 1 0
  // 4567 0 0
  // 8901 1 0
  // 1110 3 0
  println(List(
    generate("0123", 1, 0),
    generate("4567", 0, 0),
    generate("8901", 1, 0),
    generate("1110", 3, 0),
  ).reduce {
    _ intersect _
  }
  )

  // 7 - Bad guesser
  // 1111 0 0
  //2222 1 0
  //3333 0 0
  //4444 0 0
  //5555 0 0
  //6666 0 0
  //7777 2 0
  //8888 1 0
  //2778 0 4
  //7287 2 2

  println(combine("ABCD", List(0,2), "XY"))

  println(List("B", "B", "C", "X").permutations.mkString(","))


}
