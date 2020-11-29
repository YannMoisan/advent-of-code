import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day8 extends SinglePuzzle[Int, Int] {
  case class Node(
    val nbChild: Int,
    val nbMeta: Int,
    val child: ArrayBuffer[Node] = ArrayBuffer.empty[Node],
    val meta: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  )

  override def part1: String => Int = { s =>
    val numbers: Array[Int] = s.split(" ").map(_.toInt)

    var sum = 0
    var i = 2
    val stack = new mutable.Stack[Node]()
    val root = Node(numbers(0), numbers(1))
    stack.push(root)

    // DFS, iterative
    while (i < numbers.length) {
      val cur = stack.top
      if (cur.child.length == cur.nbChild) { // all children read ?
        val meta = numbers.slice(i, i + cur.nbMeta)
        sum += meta.sum
        cur.meta ++= meta
        val done = stack.pop()
        if (!stack.isEmpty) {
          stack.top.child += done
        }
        i += cur.nbMeta
      } else {
        val node = Node(numbers(i), numbers(i + 1))
        stack.push(node)
        i += 2
      }
    }
    sum
  }

  override def part2: String => Int = { s =>
    //val s2 = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    val numbers: Array[Int] = s.split(" ").map(_.toInt)

    var sum = 0
    var i = 2
    val stack = new mutable.Stack[Node]()
    val root = Node(numbers(0), numbers(1))
    stack.push(root)

    // DFS, iterative
    while (i < numbers.length) {
      val cur = stack.top
      if (cur.child.length == cur.nbChild) { // all children read ?
        val meta = numbers.slice(i, i + cur.nbMeta)
        sum += meta.sum
        cur.meta ++= meta
        val done = stack.pop()

        if (!stack.isEmpty) {
          stack.top.child += done
        }

        i += cur.nbMeta
      } else {
        val node = Node(numbers(i), numbers(i + 1))
        stack.push(node)
        i += 2
      }
    }

    score(root)
  }

  def score(n: Node) : Int = {
    if (n.nbChild == 0) n.meta.sum
    else n.meta.filter(_ <= n.nbChild).map(v => score(n.child(v-1))).sum

  }
}
