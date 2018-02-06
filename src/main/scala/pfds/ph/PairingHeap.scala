package pfds.ph

import pfds.HeapAlg

sealed trait PairingHeap[+A]

case object Leaf extends PairingHeap[Nothing]

case class Node[A](x: A, hs: List[Node[A]]) extends PairingHeap[A]

object PairingHeap {
  implicit val instance: HeapAlg[PairingHeap] = new HeapAlg[PairingHeap] {
    override def empty[A: Ordering]: PairingHeap[A] =
      Leaf

    override def isEmpty[A: Ordering](heap: PairingHeap[A]): Boolean =
      heap == Leaf

    override def insert[A: Ordering](heap: PairingHeap[A])(elem: A): PairingHeap[A] =
      merge(Node(elem, Nil))(heap)

    override def merge[A](left: PairingHeap[A])(right: PairingHeap[A])(implicit ordering: Ordering[A]): PairingHeap[A] =
      (left, right) match {
        case (h, Leaf) => h
        case (Leaf, h) => h
        case (h1@Node(x, hs1), h2@Node(y, hs2)) =>
          import ordering._
          if (x <= y) Node(x, h2 :: hs1) else Node(y, h1 :: hs2)
      }

    override def findMin[A: Ordering](heap: PairingHeap[A]): Option[A] = heap match {
      case Node(x, _) => Some(x)
      case Leaf => None
    }

    def mergePairs[A: Ordering](nodes: List[Node[A]]): PairingHeap[A] = nodes match {
      case Nil => Leaf
      case h :: Nil => h
      case h1 :: h2 :: hs => merge(merge(h1)(h2))(mergePairs(hs))
    }

    override def deleteMin[A: Ordering](heap: PairingHeap[A]): PairingHeap[A] = heap match {
      case Leaf => Leaf
      case Node(_, hs) => mergePairs(hs)
    }
  }
}
