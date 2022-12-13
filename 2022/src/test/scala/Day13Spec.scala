import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"
  //== Pair 1 ==
  //- Compare [1,1,3,1,1] vs [1,1,5,1,1]
  //  - Compare 1 vs 1
  //  - Compare 1 vs 1
  //  - Compare 3 vs 5
  //    - Left side is smaller, so inputs are in the right order
  //
  it should "work on pair 1" in {
    Day13.compare("[1,1,3,1,1]", "[1,1,5,1,1]") shouldBe -1
  }

  //== Pair 2 ==
  //- Compare [[1],[2,3,4]] vs [[1],4]
  //  - Compare [1] vs [1]
  //    - Compare 1 vs 1
  //  - Compare [2,3,4] vs 4
  //    - Mixed types; convert right to [4] and retry comparison
  //    - Compare [2,3,4] vs [4]
  //      - Compare 2 vs 4
  //        - Left side is smaller, so inputs are in the right order
  //
  it should "work on pair 2" in {
    Day13.compare("[[1],[2,3,4]]", "[[1],4]") shouldBe -1
  }

  //== Pair 3 ==
  //- Compare [9] vs [[8,7,6]]
  //  - Compare 9 vs [8,7,6]
  //    - Mixed types; convert left to [9] and retry comparison
  //    - Compare [9] vs [8,7,6]
  //      - Compare 9 vs 8
  //        - Right side is smaller, so inputs are not in the right order
  //
  it should "work on pair 3" in {
    Day13.compare("[9]", "[[8,7,6]]") shouldBe 1
  }

  //== Pair 4 ==
  //- Compare [[4,4],4,4] vs [[4,4],4,4,4]
  //  - Compare [4,4] vs [4,4]
  //    - Compare 4 vs 4
  //    - Compare 4 vs 4
  //  - Compare 4 vs 4
  //  - Compare 4 vs 4
  //  - Left side ran out of items, so inputs are in the right order
  //
  it should "work on pair 4" in {
    Day13.compare("[[4,4],4,4]", "[[4,4],4,4,4]") shouldBe -1
  }

  //== Pair 5 ==
  //- Compare [7,7,7,7] vs [7,7,7]
  //  - Compare 7 vs 7
  //  - Compare 7 vs 7
  //  - Compare 7 vs 7
  //  - Right side ran out of items, so inputs are not in the right order
  //
  it should "work on pair 5" in {
    Day13.compare("[7,7,7,7]", "[7,7,7]") shouldBe 1
  }

  //== Pair 6 ==
  //- Compare [] vs [3]
  //  - Left side ran out of items, so inputs are in the right order
  //
  it should "work on pair 6" in {
    Day13.compare("[]", "[3]") shouldBe -1
  }

  //== Pair 7 ==
  //- Compare [[[]]] vs [[]]
  //  - Compare [[]] vs []
  //    - Right side ran out of items, so inputs are not in the right order
  //
  it should "work on pair 7" in {
    Day13.compare("[[[]]]", "[[]]") shouldBe 1
  }

  //== Pair 8 ==
  //- Compare [1,[2,[3,[4,[5,6,7]]]],8,9] vs [1,[2,[3,[4,[5,6,0]]]],8,9]
  //  - Compare 1 vs 1
  //  - Compare [2,[3,[4,[5,6,7]]]] vs [2,[3,[4,[5,6,0]]]]
  //    - Compare 2 vs 2
  //    - Compare [3,[4,[5,6,7]]] vs [3,[4,[5,6,0]]]
  //      - Compare 3 vs 3
  //      - Compare [4,[5,6,7]] vs [4,[5,6,0]]
  //        - Compare 4 vs 4
  //        - Compare [5,6,7] vs [5,6,0]
  //          - Compare 5 vs 5
  //          - Compare 6 vs 6
  //          - Compare 7 vs 0
  //            - Right side is smaller, so inputs are not in the right order
  it should "work on pair 8" in {
    Day13.compare("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[[]]") shouldBe 1
  }
}

