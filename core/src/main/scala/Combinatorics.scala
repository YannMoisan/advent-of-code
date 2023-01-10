import scala.collection.mutable

object Combinatorics extends App {

  def permutations2[A](l: List[A], k: Int): List[List[A]] = {
    // dfs
    val b = mutable.Buffer[List[A]]()
    _permutations2(Nil, Set(), 0)

    def _permutations2(cur: List[A], used: Set[A], depth: Int): Unit = {
      if (depth == k) {
        b += cur
      } else {
        for {
          e <- l
          if (!used.contains(e))
        } {
          _permutations2(e :: cur, used + e, depth + 1)
          // backtrack
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

  println(permutations2(List("A", "B", "C", "D"), 2))
  println(permutations2(List("A", "B", "C", "D"), 2).size)
}
