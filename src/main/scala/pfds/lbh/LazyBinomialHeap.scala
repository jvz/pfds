package pfds.lbh

import cats._
import pfds.HeapAlg

import scala.annotation.tailrec

case class LazyBinomialHeap[A](trees: Eval[Forest[A]]) {
  def map(f: Forest[A] => Forest[A]): LazyBinomialHeap[A] = copy(trees map f)
}

sealed trait Forest[+A] {
  def isEmpty: Boolean

  def ::[A0 >: A](t: Node[A0]): Forest[A0] = TreeCons(t, this)

  def :+[A0 >: A : Ordering](x: A0): Forest[A0] = this :+ Node(0, x, Empty)

  // insTree
  def :+[A0 >: A : Ordering](t: Node[A0]): Forest[A0]

  // insTree
  def +:[A0 >: A : Ordering](t: Node[A0]): Forest[A0] = this :+ t

  // mrg
  def ++[A0 >: A : Ordering](that: Forest[A0]): Forest[A0]

  def splitAtMin[A0 >: A : Ordering]: Option[(Node[A0], Forest[A0])]

  def reverse: Forest[A]
}

case object Empty extends Forest[Nothing] {
  override val isEmpty: Boolean = true

  override def :+[A0: Ordering](t: Node[A0]): Forest[A0] = TreeCons(t, Empty)

  override def ++[A0: Ordering](that: Forest[A0]): Forest[A0] = that

  override def splitAtMin[A0: Ordering]: Option[(Node[A0], Forest[A0])] = None

  override def reverse: Forest[Nothing] = Empty
}

case class TreeCons[+A](head: Node[A], tail: Forest[A]) extends Forest[A] {
  override val isEmpty: Boolean = false

  override def :+[A0 >: A : Ordering](t: Node[A0]): Forest[A0] =
    if (t.rank < head.rank) t :: this else (t link head) +: tail

  override def ++[A0 >: A : Ordering](that: Forest[A0]): Forest[A0] = that match {
    case Empty => this
    case TreeCons(head2, tail2) =>
      if (head.rank < head2.rank) head :: tail ++ that
      else if (head2.rank < head.rank) head2 :: this ++ tail2
      else (head link head2) +: (tail ++ tail2)
  }

  override def splitAtMin[A0 >: A](implicit ordering: Ordering[A0]): Option[(Node[A0], Forest[A0])] = tail match {
    case Empty => Some((head, tail))
    case TreeCons(t, ts) =>
      import ordering._
      for ((tP, tsP) <- ts.splitAtMin) yield if (t.value <= tP.value) (t, ts) else (tP, t :: tsP)
  }

  override def reverse: Forest[A] = {
    @tailrec
    def go(xs: Forest[A], acc: Forest[A]): Forest[A] = xs match {
      case Empty => acc
      case TreeCons(hd, tl) => go(tl, TreeCons(hd, acc))
    }

    go(this, Empty)
  }
}

case class Node[+A](rank: Int, value: A, children: Forest[A]) {
  def link[A0 >: A](that: Node[A0])(implicit ordering: Ordering[A0]): Node[A0] = {
    import ordering._
    if (this.value <= that.value) this.copy(rank = rank + 1, children = that :: children)
    else that.copy(rank = this.rank + 1, children = this :: that.children)
  }
}

object LazyBinomialHeap {
  implicit val instance: HeapAlg[LazyBinomialHeap] = new HeapAlg[LazyBinomialHeap] {
    override def empty[A](implicit ordering: Ordering[A]): LazyBinomialHeap[A] =
      LazyBinomialHeap(Now(Empty))

    override def isEmpty[A](heap: LazyBinomialHeap[A])(implicit ordering: Ordering[A]): Boolean =
      heap.trees.value.isEmpty

    override def insert[A](heap: LazyBinomialHeap[A])(elem: A)(implicit ordering: Ordering[A]): LazyBinomialHeap[A] =
      heap.map(_ :+ elem)

    override def merge[A](left: LazyBinomialHeap[A])(right: LazyBinomialHeap[A])(implicit ordering: Ordering[A]): LazyBinomialHeap[A] =
      LazyBinomialHeap {
        for {
          ts1 <- left.trees
          ts2 <- right.trees
        } yield ts1 ++ ts2
      }

    override def findMin[A](heap: LazyBinomialHeap[A])(implicit ordering: Ordering[A]): Option[A] =
      for ((t, _) <- heap.trees.value.splitAtMin) yield t.value

    override def deleteMin[A](heap: LazyBinomialHeap[A])(implicit ordering: Ordering[A]): LazyBinomialHeap[A] =
      heap map { ts =>
        (for ((Node(_, _, ts1), ts2) <- ts.splitAtMin) yield {
          ts1.reverse ++ ts2
        }).getOrElse(Empty)
      }
  }
}
