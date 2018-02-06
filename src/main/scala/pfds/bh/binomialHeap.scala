package pfds.bh

import pfds.HeapAlg

import scala.annotation.tailrec

sealed trait BinomialHeap[+A] {
  private[bh]def insNode[A0 >: A : Ordering](node: Node[A0]): BinomialHeap[A0]

  private[bh] def removeMinTree[A0 >: A](implicit ordering: Ordering[A0]): Option[(Node[A0], BinomialHeap[A0])]
}

case object Leaf extends BinomialHeap[Nothing] {
  override private[bh]def insNode[A0: Ordering](node: Node[A0]): BinomialHeap[A0] = node

  override private[bh] def removeMinTree[A0 >: Nothing](implicit ordering: Ordering[A0]) = None
}

case class Node[+A](rank: Int, value: A, child: BinomialHeap[A], sibling: BinomialHeap[A]) extends BinomialHeap[A] {
  override private[bh]def insNode[A0 >: A : Ordering](node: Node[A0]): BinomialHeap[A0] =
    if (rank > node.rank) node.copy(sibling = this) else sibling insNode (node link this)

  // maintain heap order by linking trees with larger roots under trees with smaller roots
  // always link trees of equal rank
  // this function is analogous to a carry operation in binary addition
  private[bh] def link[A0 >: A](node: Node[A0])(implicit ordering: Ordering[A0]): Node[A0] = {
    import ordering._
    if (value <= node.value) insChild(node) else node.insChild(this)
  }

  private[bh] def insChild[A0 >: A](node: Node[A0]): Node[A0] =
    copy(rank = this.rank + 1, child = node.copy(sibling = this.child))

  override private[bh] def removeMinTree[A0 >: A](implicit ordering: Ordering[A0]): Option[(Node[A0], BinomialHeap[A0])] =
    sibling match {
      case Leaf => Some((this, Leaf))
      case Node(_, _, _, _) =>
        import ordering._
        for ((t, ts) <- sibling.removeMinTree[A0]) yield {
          if (value <= t.value) (this, sibling) else (t, ts insNode this)
        }
    }
}

object Node {
  def pure[A](a: A) = Node(0, a, Leaf, Leaf)
}

object BinomialHeap {
  implicit val instance: HeapAlg[BinomialHeap] = new HeapAlg[BinomialHeap] {
    override def empty[A](implicit ordering: Ordering[A]): BinomialHeap[A] = Leaf

    override def isEmpty[A](heap: BinomialHeap[A])(implicit ordering: Ordering[A]): Boolean = heap == Leaf

    // now we have a list of trees where where the index corresponds to a tree of rank 2^i
    // this is rather similar to binary numbers, so we'll be implementing insert and merge
    // similar to binary addition
    override def insert[A](heap: BinomialHeap[A])(elem: A)(implicit ordering: Ordering[A]): BinomialHeap[A] =
      heap insNode Node.pure(elem)

    override def merge[A](left: BinomialHeap[A])(right: BinomialHeap[A])(implicit ordering: Ordering[A]): BinomialHeap[A] =
      (left, right) match {
        case (h, Leaf) => h
        case (Leaf, h) => h
        case (h1: Node[A], h2: Node[A]) =>
          if (h1.rank < h2.rank) h1.copy(sibling = merge(h1.sibling)(h2))
          else if (h1.rank > h2.rank) h2.copy(sibling = merge(h1)(h2.sibling))
          else merge(h1.sibling)(h2.sibling) insNode (h1 link h2)
      }

    // exercise 3.5
    override def findMin[A](heap: BinomialHeap[A])(implicit ordering: Ordering[A]): Option[A] = heap match {
      case Leaf => None
      case Node(_, value, _, Leaf) => Some(value)
      case Node(_, value, _, sibling) =>
        import ordering._
        findMin(sibling).map(_ min value)
    }

    private def reverse[A](heap: BinomialHeap[A]): BinomialHeap[A] = {
      @tailrec
      def go(orig: BinomialHeap[A], rev: BinomialHeap[A]): BinomialHeap[A] = orig match {
        case Leaf => rev
        case Node(rank, value, child, sibling) => go(sibling, Node(rank, value, child, rev))
      }

      go(heap, Leaf)
    }

    override def deleteMin[A](heap: BinomialHeap[A])(implicit ordering: Ordering[A]): BinomialHeap[A] = {
      for ((t, ts) <- heap.removeMinTree) yield merge(reverse(ts))(t.sibling)
    }.getOrElse(Leaf)
  }

  def apply[A: Ordering](xs: A*): BinomialHeap[A] =
    xs.foldLeft(instance.empty[A]) { (acc, x) =>
      instance.insert(acc)(x)
    }
}
