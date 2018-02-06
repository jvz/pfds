package pfds.sh

import pfds.HeapAlg

import scala.annotation.tailrec

sealed trait SplayHeap[+A]

case object Leaf extends SplayHeap[Nothing]

case class Node[+A](x: A, a: SplayHeap[A], b: SplayHeap[A]) extends SplayHeap[A]

object SplayHeap {
  implicit val instance: HeapAlg[SplayHeap] = new HeapAlg[SplayHeap] {
    override def empty[A](implicit ordering: Ordering[A]): SplayHeap[A] = Leaf

    override def isEmpty[A](heap: SplayHeap[A])(implicit ordering: Ordering[A]): Boolean = heap == Leaf

    def bigger[A](heap: SplayHeap[A])(pivot: A)(implicit ordering: Ordering[A]): SplayHeap[A] = heap match {
      case Leaf => heap
      case Node(x, a, b) =>
        import ordering._
        if (x <= pivot) bigger(b)(pivot) else a match {
          case Leaf => heap
          case Node(y, a1, a2) =>
            if (y <= pivot) Node(x, bigger(a2)(pivot), b)
            else Node(y, bigger(a1)(pivot), Node(x, a2, b))
        }
    }

    // exercise 5.4
    def smaller[A](heap: SplayHeap[A])(pivot: A)(implicit ordering: Ordering[A]): SplayHeap[A] = heap match {
      case Leaf => heap
      case Node(x, a, b) =>
        import ordering._
        if (x > pivot) smaller(a)(pivot) else b match {
          case Leaf => heap
          case Node(y, b1, b2) =>
            if (y > pivot) Node(x, a, smaller(b1)(pivot))
            else Node(y, Node(x, a, b1), smaller(b2)(pivot))
        }
    }

    // original implementation of insert
    def insertOrig[A](heap: SplayHeap[A])(elem: A)(implicit ordering: Ordering[A]): SplayHeap[A] =
      Node(elem, smaller(heap)(elem), bigger(heap)(elem))

    def partition[A](heap: SplayHeap[A])(pivot: A)(implicit ordering: Ordering[A]): (SplayHeap[A], SplayHeap[A]) = {
      import ordering._
      def go(t: SplayHeap[A]): (SplayHeap[A], SplayHeap[A]) = t match {
        case Leaf => (Leaf, Leaf)
        case Node(x, a, b) =>
          if (x <= pivot) b match {
            case Leaf => (t, Leaf)
            case Node(y, b1, b2) =>
              if (y <= pivot) {
                val (small, big) = go(b2)
                (Node(y, Node(x, a, b1), small), big)
              } else {
                val (small, big) = go(b1)
                (Node(x, a, small), Node(y, big, b2))
              }
          } else a match {
            case Leaf => (Leaf, t)
            case Node(y, a1, a2) =>
              if (y <= pivot) {
                val (small, big) = go(a2)
                (Node(y, a1, small), Node(x, big, b))
              } else {
                val (small, big) = go(a1)
                (small, Node(y, big, Node(x, a2, b)))
              }
          }
      }

      go(heap)
    }

    override def insert[A](heap: SplayHeap[A])(elem: A)(implicit ordering: Ordering[A]): SplayHeap[A] = {
      val (small, big) = partition(heap)(elem)
      Node(elem, small, big)
    }

    override def merge[A](left: SplayHeap[A])(right: SplayHeap[A])(implicit ordering: Ordering[A]): SplayHeap[A] =
      left match {
        case Leaf => right
        case Node(x, a, b) =>
          val (ta, tb) = partition(right)(x)
          Node(x, merge(ta)(a), merge(tb)(b))
      }

    @tailrec
    override def findMin[A](heap: SplayHeap[A])(implicit ordering: Ordering[A]): Option[A] = heap match {
      case Node(x, a, _) =>
        if (a == Leaf) Some(x) else findMin(a)
      case Leaf => None
    }

    override def deleteMin[A](heap: SplayHeap[A])(implicit ordering: Ordering[A]): SplayHeap[A] = heap match {
      case Node(_, Leaf, b) => b
      case Node(y, Node(x, a, b), c) =>
        if (a == Leaf) Node(y, b, c) else Node(x, deleteMin(a), Node(y, b, c))
      case Leaf => Leaf
    }
  }
}
