package com.yannmoisan.util.collection

class DoubleLinkedListNode[A](
    var value: A,
    var prev: DoubleLinkedListNode[A],
    var next: DoubleLinkedListNode[A]
) {
  // A => (B) = C becomes A => C
  def remove(): A = {
    prev.next = next
    next.prev = prev
    value
  }

  // (A) => B becomes A => N => B
  def addAfter(value: A): Unit = {
    val newNode = new DoubleLinkedListNode(value, this, this.next)
    this.next.prev = newNode
    this.next = newNode
  }
}

object DoubleLinkedListNode {
  def single[A](value: A): DoubleLinkedListNode[A] = {
    val newNode = new DoubleLinkedListNode[A](value, null, null)
    newNode.prev = newNode
    newNode.next = newNode
    newNode
  }
}
