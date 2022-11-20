# Basic combinatorics in Scala

Python has all the basic combinatorics methods in itertools.
In scala, some of them are missing from the standard library.

## k-combinations

```
                      n!
Number of items =  -------
                   k!(n-k)!
```

The method is in the standard library

```
scala> List("A", "B", "C", "D").combinations(2).toList

val res5: List[List[String]] = List(List(A, B), List(A, C), List(A, D), List(B, C), List(B, D), List(C, D))
```

It can be implemented with a recursive method

```
  def combinations[A](l: List[A], k: Int): List[List[A]] =
    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => combinations(t, k) ++ combinations(t, k - 1).map(l => h :: l)
        case _      => Nil
      }
    }
```

## k-combinations with replacement

```
                   (n+k-1)!
Number of items =  --------
                   k!(n-1)!
```

The above method can be adapted

```
  def combinationsWithReplacement[A](l: List[A], k: Int): List[List[A]] =
    if (k == 1) l.map(a => List(a))
    else {
      l match {
        case h :: t => combinationsWithReplacement(t, k) ++ combinationsWithReplacement(l, k - 1).map(l => h :: l)
        case _ => Nil
      }
    }
```

## k-permutations (arrangement in french)

```
                     n!
Number of items =  ------
                   (n-k)!
```

It can be implemented with a recursive method

```
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
```

## k-permutations with replacement

It is the equivalent to a dot product

```
Number of items = n^k
```

It can be implemented with a recursive method

```
  def permutationsWithReplacement[A](l: List[A], k: Int): List[List[A]] = {
    if (k == 1) l.map(List(_)) else
    l.flatMap(e => permutationsWithReplacement(l, k-1).map(ll => e :: ll))
  }
```

## Permutations with k=n

```
Number of results = n!
```

It exists a builtin method 

```
scala> List("A", "B", "C", "D").permutations.toList

val res9: List[List[String]] = List(List(A, B, C, D), List(A, B, D, C), List(A, C, B, D), List(A, C, D, B), List(A, D, B, C), List(A, D, C, B), List(B, A, C, D), List(B, A, D, C), List(B, C, A, D), List(B, C, D, A), List(B, D, A, C), List(B, D, C, A), List(C, A, B, D), List(C, A, D, B), List(C, B, A, D), List(C, B, D, A), List(C, D, A, B), List(C, D, B, A), List(D, A, B, C), List(D, A, C, B), List(D, B, A, C), List(D, B, C, A), List(D, C, A, B), List(D, C, B, A))
```

## Powerset

```
Number of results = 2^n
```

It exists a builtin method

```
scala> Set("A", "B", "C", "D").subsets.toList

val res10: List[scala.collection.immutable.Set[String]] = List(Set(), Set(A), Set(B), Set(C), Set(D), Set(A, B), Set(A, C), Set(A, D), Set(B, C), Set(B, D), Set(C, D), Set(A, B, C), Set(A, B, D), Set(A, C, D), Set(B, C, D), Set(A, B, C, D))
```

# day X

5 * 6 * 22 = 660