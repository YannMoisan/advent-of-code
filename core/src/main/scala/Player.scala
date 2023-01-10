  package tt

  import scala.collection.mutable
  import scala.io.StdIn.readLine

  // https://www.codingame.com/ide/puzzle/bulls-and-cows-2
  object Player extends App {
    val numberLength = readLine().toInt

    var lastGuess : String = _
    val guesses = mutable.Buffer  [Guess]()

    val candidates = Combinatorics
      .permutations3(('0' to '9').toList, numberLength)
      .map(xs => new String(xs.toArray))
      .filter(_.head != '0')

    // game loop
    while(true) {
      val Array(bulls, cows) = (readLine() split " ").filter(_ != "").map (_.toInt)
      val s = System.nanoTime()
      if (bulls != -1) {
        guesses += Guess(lastGuess, bulls, cows)
      }

      lastGuess = if (guesses.isEmpty) {
        "1234567890".take(numberLength)
      } else
        BruteForce.solve(guesses.toList)



      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
      val e = System.nanoTime()
      Console.err.println(s"${(e-s)/1000000}ms")
      println(lastGuess) // number with numberLength count of digits
    }
  }

  case class Guess(guess: String, bulls: Int, cows: Int)

  trait BullsAndCows {
    def solve(guesses: List[Guess]) : String
  }

  object BruteForce extends BullsAndCows {
    override def solve(guesses: List[Guess]): String = {
      Player.candidates.find { candidate =>
        guesses.forall { guess => evaluate(candidate, guess.guess) == ((guess.bulls, guess.cows)) }
      }.get
    }

    private def evaluate(sol: String, guess: String): (Int, Int) = {
      // Find bulls
      var bulls = 0
      var cows = 0
      // populate the state for the algo
      val state = Array.ofDim[Int](10)
      //mutable.Map[Char, Int]()
      sol.foreach { c =>
        val index = c - '0'
        state(index) = state(index) + 1
      }

      val len = sol.length

      var i = 0
      while (i < len) {
        val index = guess.charAt(i) - '0'
        if (sol.charAt(i) == guess.charAt(i)) {
          bulls += 1
        } else if (state(index) > 0) {
          cows += 1
        }
        i += 1
      }
      (bulls, cows)
    }

  }

  object Combinatorics extends App {

    def permutations3(l: List[Char], k: Int): List[List[Char]] = {
      // dfs
      val b = mutable.Buffer[List[Char]]()
      val used = Array.ofDim[Boolean](10)
      _permutations3(Nil, 0)

      def _permutations3(cur: List[Char], depth: Int): Unit = {
        if (depth == k) {
          b += cur
        } else {
          for {
            e <- l
            if (!used(e - '0'))
          } {
            val idx = e - '0'
            used(idx) = true
            _permutations3(e :: cur, depth + 1)
            // backtrack
            used(idx) = false
          }
        }
      }

      b.toList

    }

    def permutations4(l: List[Char], k: Int): List[String] = {
      // dfs
      val b = mutable.Buffer[String]()
      val used = Array.ofDim[Boolean](10)
      _permutations4("", 0)

      def _permutations4(cur: String, depth: Int): Unit = {
        if (depth == k) {
          b += cur
        } else {
          for {
            e <- l
            if (!used(e - '0'))
          } {
            val idx = e - '0'
            used(idx) = true
            _permutations4(cur + e, depth + 1)
            // backtrack
            used(idx) = false
          }
        }
      }

      b.toList

    }

    def permutations2[A](l: List[A], k: Int): List[List[A]] = {
      // dfs
      val b = mutable.Buffer[List[A]]()
      val used = mutable.Set[A]()
      _permutations2(Nil, used /*Set()*/ , 0)

      def _permutations2(cur: List[A], used: mutable.Set[A], depth: Int): Unit = {
        if (depth == k) {
          b += cur
        } else {
          for {
            e <- l
            if (!used.contains(e))
          } {
            _permutations2(e :: cur, used += e, depth + 1)
            // backtrack
            used.remove(e)
          }
        }
      }

      b.toList

    }

    def permutations[A](l: List[A], k: Int): List[List[A]] = {
      def combine(e: A, l: List[List[A]]) : List[List[A]] = {
        l.flatMap { ll =>
          (0 to ll.length).map { i =>
            val (pre, suf) = ll.splitAt(i)
            pre ++ List(e) ++ suf
          }
        }
      }

      if (k == 1) l.map(a => List(a))
      else {
        l match {
          case h :: t => permutations(t, k) ++ combine(h, permutations(t, k - 1))
          case _ => Nil
        }
      }
    }

    /** @return successive r length permutations of elements in the list. */
    def permutationsWithReplacement[A](l: List[A], k: Int): List[List[A]] = {
      if (k == 1) l.map(List(_)) else
        l.flatMap(e => permutationsWithReplacement(l, k-1).map(ll => e :: ll))
    }

    /** @return r length subsequences of elements from the input list. */
    def combinations[A](l: List[A], k: Int): List[List[A]] =
      if (k == 1) l.map(a => List(a))
      else {
        l match {
          case h :: t => combinations(t, k) ++ combinations(t, k - 1).map(l => h :: l)
          case _      => Nil
        }
      }

    /** @return r length subsequences of elements from the input list allowing individual elements to be repeated more than once. */
    def combinationsWithReplacement[A](l: List[A], k: Int): List[List[A]] =
      if (k == 1) l.map(a => List(a))
      else {
        l match {
          case h :: t => combinationsWithReplacement(t, k) ++ combinationsWithReplacement(l, k - 1).map(l => h :: l)
          case _ => Nil
        }
      }

    List(2) ::: List(1)

    println(permutationsWithReplacement(List("A", "B", "C", "D"), 2))
    println(permutationsWithReplacement(List("A", "B", "C", "D"), 2).size)

    println(permutations(List("A", "B", "C", "D"), 2))
    println(permutations(List("A", "B", "C", "D"), 2).size)

    println(combinationsWithReplacement(List("A", "B", "C", "D"), 2))
    println(combinationsWithReplacement(List("A", "B", "C", "D"), 2).size)

    println(combinations(List("A", "B", "C", "D"), 2))
    println(combinations(List("A", "B", "C", "D"), 2).size)
  }
