package pfds.bq

import pfds.DequeAlg

// exercise 5.1
// new invariant:
// both f and r must be non-empty when there are two or more elements
// when one becomes empty, we split the other list in half and reverse one of the halves
case class BatchedDeque[A](f: List[A], r: List[A]) {
  def checked: BatchedDeque[A] =
    if (f.isEmpty) {
      val n = r.size
      if (n <= 1) this else {
        val (rear, front) = r.splitAt(n / 2)
        BatchedDeque(front.reverse, rear)
      }
    } else if (r.isEmpty) {
      val n = f.size
      if (n <= 1) this else {
        val (front, rear) = f.splitAt(n / 2)
        BatchedDeque(front, rear.reverse)
      }
    } else this
}

object BatchedDeque {
  implicit val instance: DequeAlg[BatchedDeque] = new DequeAlg[BatchedDeque] {
    override def empty[A]: BatchedDeque[A] =
      BatchedDeque(Nil, Nil)

    override def isEmpty[A](xs: BatchedDeque[A]): Boolean =
      xs.f.isEmpty && xs.r.isEmpty

    override def cons[A](head: A)(tail: BatchedDeque[A]): BatchedDeque[A] =
      tail.copy(f = head :: tail.f).checked

    override def head[A](xs: BatchedDeque[A]): A =
      if (xs.f.nonEmpty) xs.f.head else xs.r.last

    override def tail[A](xs: BatchedDeque[A]): BatchedDeque[A] =
      xs.copy(f = xs.f.tail).checked

    override def snoc[A](init: BatchedDeque[A])(last: A): BatchedDeque[A] =
      init.copy(r = last :: init.r).checked

    override def init[A](xs: BatchedDeque[A]): BatchedDeque[A] =
      xs.copy(r = xs.r.tail)

    override def last[A](xs: BatchedDeque[A]): A =
      if (xs.r.nonEmpty) xs.r.head else xs.f.last
  }
}
