package pfds.lh

import pfds.HeapAlg

sealed trait LeftistHeap[+A]

case object Leaf extends LeftistHeap[Nothing]

case class Node[+A](rank: Int, value: A, left: LeftistHeap[A], right: LeftistHeap[A]) extends LeftistHeap[A]

object LeftistHeap {
  implicit val instance: HeapAlg[LeftistHeap] = new HeapAlg[LeftistHeap] {
    override def empty[A](implicit ordering: Ordering[A]): LeftistHeap[A] = Leaf

    override def isEmpty[A](heap: LeftistHeap[A])(implicit ordering: Ordering[A]): Boolean = heap == empty

    override def insert[A](heap: LeftistHeap[A])(elem: A)(implicit ordering: Ordering[A]): LeftistHeap[A] =
      merge(Node(1, elem, empty, empty))(heap)

    override def merge[A](left: LeftistHeap[A])(right: LeftistHeap[A])(implicit ordering: Ordering[A]): LeftistHeap[A] = {
      def rank(a: LeftistHeap[A]) = a match {
        case Leaf => 0
        case Node(rank, _, _, _) => rank
      }

      def makeT(x: A, a: LeftistHeap[A], b: LeftistHeap[A]) = {
        val ra = rank(a)
        val rb = rank(b)
        if (ra >= rb) Node(rb + 1, x, a, b) else Node(ra + 1, x, b, a)
      }

      import ordering._
      (left, right) match {
        case (h, Leaf) => h
        case (Leaf, h) => h
        case (h1@Node(_, x, a1, b1), h2@Node(_, y, a2, b2)) =>
          if (x < y) makeT(x, a1, merge(b1)(h2)) else makeT(y, a2, merge(h1)(b2))
      }
    }

    override def findMin[A](heap: LeftistHeap[A])(implicit ordering: Ordering[A]): Option[A] = heap match {
      case Leaf => None
      case Node(_, a, _, _) => Some(a)
    }

    override def deleteMin[A](heap: LeftistHeap[A])(implicit ordering: Ordering[A]): LeftistHeap[A] = heap match {
      case Leaf => Leaf
      case Node(_, _, a, b) => merge(a)(b)
    }
  }
}


