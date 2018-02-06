package pfds.heap

import pfds.HeapAlg

// exercise 3.7
sealed trait ExplicitMinHeap[H[_], A]

case class EmptyHeap[H[_], A]() extends ExplicitMinHeap[H, A]

case class NonEmptyHeap[H[_], A](min: A, heap: H[A]) extends ExplicitMinHeap[H, A]

object ExplicitMinHeap {

  implicit class Instance[H[_]](alg: HeapAlg[H]) extends HeapAlg[ExplicitMinHeap[H, ?]] {
    override def empty[A](implicit ordering: Ordering[A]): ExplicitMinHeap[H, A] =
      EmptyHeap()

    override def isEmpty[A](heap: ExplicitMinHeap[H, A])(implicit ordering: Ordering[A]): Boolean =
      heap == empty

    override def insert[A](heap: ExplicitMinHeap[H, A])(elem: A)(implicit ordering: Ordering[A]): ExplicitMinHeap[H, A] =
      heap match {
        case EmptyHeap() => NonEmptyHeap(elem, alg.insert(alg.empty[A])(elem))
        case NonEmptyHeap(prevMin, h) =>
          import ordering._
          NonEmptyHeap(prevMin min elem, alg.insert(h)(elem))
      }

    override def merge[A](left: ExplicitMinHeap[H, A])(right: ExplicitMinHeap[H, A])(implicit ordering: Ordering[A]): ExplicitMinHeap[H, A] =
      (left, right) match {
        case (h, EmptyHeap()) => h
        case (EmptyHeap(), h) => h
        case (NonEmptyHeap(m1, h1), NonEmptyHeap(m2, h2)) =>
          import ordering._
          NonEmptyHeap(m1 min m2, alg.merge(h1)(h2))
      }

    override def findMin[A](heap: ExplicitMinHeap[H, A])(implicit ordering: Ordering[A]): Option[A] =
      heap match {
        case EmptyHeap() => None
        case NonEmptyHeap(min, _) => Some(min)
      }

    override def deleteMin[A](heap: ExplicitMinHeap[H, A])(implicit ordering: Ordering[A]): ExplicitMinHeap[H, A] =
      heap match {
        case EmptyHeap() => heap
        case NonEmptyHeap(_, h) =>
          val updated = alg.deleteMin(h)
          alg.findMin(updated) match {
            case Some(min) => NonEmptyHeap(min, updated)
            case None => EmptyHeap()
          }
      }
  }

}
