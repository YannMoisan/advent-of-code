import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {
  behavior of "explode"

  it should "work on example 1" in {
    Day18.explode("[[[[[9,8],1],2],3],4]") shouldEqual "[[[[0,9],2],3],4]"
  }

  it should "work on example 2" in {
    Day18.explode("[7,[6,[5,[4,[3,2]]]]]") shouldEqual "[7,[6,[5,[7,0]]]]"
  }

  it should "work on example 3" in {
    Day18.explode("[[6,[5,[4,[3,2]]]],1]") shouldEqual "[[6,[5,[7,0]]],3]"
  }

  it should "work on example 4" in {
    Day18.explode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") shouldEqual "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
  }

  it should "work on example 5" in {
    Day18.explode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") shouldEqual "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
  }

  it should "work on big example - explode 1" in {
    Day18.explode("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") shouldEqual "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
  }

  it should "work on big example - explode 2" in {
    Day18.explode("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]") shouldEqual
      "[[[[0,7],4],[15,[0,13]]],[1,1]]"
  }

  behavior of "split"

  it should "work on big example - split 1" in {
    Day18.split("[[[[0,7],4],[15,[0,13]]],[1,1]]") shouldEqual "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
  }

//    [[[[0,7],4],[[15],[0,13]]],[1,1]]" did not equal "
//    [[[[0,7],4],[[[7,8]],[0,13]]],[1,1]]

  it should "work on big example - split 2" in {
    Day18.split("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") shouldEqual "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
  }

  behavior of "reduce"
  it should "work on big example" in {
    Day18.reduce("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") shouldEqual "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  }

  behavior of "add"
  it should "work on example1" in {
    val in = Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]")
    Day18.add(in) shouldEqual "[[[[1,1],[2,2]],[3,3]],[4,4]]"
  }

  it should "work on example2" in {
    val in = Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]")
    Day18.add(in) shouldEqual "[[[[3,0],[5,3]],[4,4]],[5,5]]"
  }

  it should "work on example3" in {
    val in = Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]")
    Day18.add(in) shouldEqual "[[[[5,0],[7,4]],[5,5]],[6,6]]"
  }

  it should "work on slightly larger example" in {
    val in = Seq(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
      "[7,[5,[[3,8],[1,4]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[2,9]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[4,2],2],6],[8,7]]"
    )
    Day18.add(in) shouldEqual "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
  }
}
