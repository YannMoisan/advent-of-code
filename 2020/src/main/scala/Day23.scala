import java.util.LinkedList

//import scala.jdk.CollectionConverters._
//asJavaIterableConverter

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.TraversableOps"))
object Day23 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val ll = new LinkedList[Int]()
    "643719258".map(_.toString.toInt).foreach(ll.add)

    var ptr = 0
    (1 to 100).foreach(_ => ptr = update(ll, ptr))

    val oneIndex = ll.indexOf(1)
    val indices  = (oneIndex + 1 until ll.size) ++ (0 until oneIndex)
    indices.map(ll.get).mkString.toLong
  }

  def update(ll: LinkedList[Int], ptr: Int): Int = {
    val size = ll.size

    val it = ll.listIterator(ptr)

    val currentCupLabel = it.next()

    val a = if (it.hasNext) {
      val v = it.next
      val _ = it.previous()
      it.remove()
      v
    } else ll.removeFirst()

    val b = if (it.hasNext) {
      val v = it.next
      val _ = it.previous()
      it.remove()
      v
    } else ll.removeFirst()

    val c = if (it.hasNext) {
      val v = it.next
      val _ = it.previous()
      it.remove()
      v
    } else ll.removeFirst()

//    if (it.hasNext) it.remove() else ll.removeFirst()
//    if (it.hasNext) it.remove() else ll.removeFirst()

//    val currentCupLabel = ll.get(ptr)
//
//    val a = ll.remove(if (ptr + 1 < ll.size) ptr + 1 else 0)
//    val b = ll.remove(if (ptr + 1 < ll.size) ptr + 1 else 0)
//    val c = ll.remove(if (ptr + 1 < ll.size) ptr + 1 else 0)

    //val pickUpValues = Seq(a, b, c)

    // find destination
    var dest   = -1
    var target = currentCupLabel - 1
    while (target >= 1 && dest == -1) {
      if (target == a || target == b || target == c) {
        target = target - 1
      } else {
        dest = target
      }
    }
    if (dest == -1) {
      dest = (size - 2 to size).toSet.diff(Set(a, b, c)).max
    }

    val destIndex = ll.indexOf(dest)

    val it2 = ll.listIterator(destIndex + 1)
    //while (it2.hasNext && it2.next() != dest) {}
    //it2.next()
    it2.add(a)
    it2.add(b)
    it2.add(c)

    //val _         = ll.addAll(destIndex + 1, pickUpValues.asJava)
//    ll.add(destIndex + 1, b)
//    ll.add(destIndex + 1, a)

    //if (it.hasNext) it.nextIndex() else 0

//    if (destIndex > ptr) (ptr + 1) % ll.size
//    else (ptr + 4)                 % ll.size

    (ll.indexOf(currentCupLabel) + 1) % ll.size
  }

  override def part2(input: Iterator[String]): Long = {
    val ll = new LinkedList[Int]()
    ("643719258".map(_.toString.toInt) ++ (10 to 1_000_000)).foreach(ll.add)

    var ptr = 0
    (1 to 10_000_000).foreach(_ => ptr = update(ll, ptr))

    //10000000  => 100000s = 27h

    val oneIndex = ll.indexOf(1)
    ll.get(oneIndex + 1).toLong * ll.get(oneIndex + 2).toLong
    //    val ll = new LinkedList[Int]()
    //    ("643719258".map(_.toString.toInt) ++ (10 to 1_000_000)).foreach(ll.add)
    //    val ptr = 0
    //42L
    //    val state    = State("643719258".map(_.toString.toInt) ++ (10 to 1_000_000), 0)
    //    val end      = Iterator.iterate(state)(next).drop(100).next()
    //    val oneIndex = end.cups.indexOf(1)
    //    end.cups(oneIndex + 1).toLong * end.cups(oneIndex + 2).toLong
  }
}
