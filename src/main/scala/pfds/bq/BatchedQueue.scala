package pfds.bq

import pfds.QueueAlg

// add elements to rear (which is in reverse order), remove elements from front
// whenever f would become empty, take r, reverse it, and set f to that, and set r to empty
// this maintains the invariant property that f is empty only if r is empty
case class BatchedQueue[A](front: List[A], rear: List[A])

object BatchedQueue {
  implicit val instance: QueueAlg[BatchedQueue] = new QueueAlg[BatchedQueue] {
    override def empty[A]: BatchedQueue[A] =
      BatchedQueue(Nil, Nil)

    override def isEmpty[A](xs: BatchedQueue[A]): Boolean =
      xs.front.isEmpty

    def checkf[A](xs: BatchedQueue[A]): BatchedQueue[A] =
      if (xs.front.isEmpty) BatchedQueue(xs.rear.reverse, Nil) else xs

    override def snoc[A](xs: BatchedQueue[A])(x: A): BatchedQueue[A] =
      checkf(xs.copy(rear = x :: xs.rear))

    override def head[A](xs: BatchedQueue[A]): A =
      xs.front.head

    override def tail[A](xs: BatchedQueue[A]): BatchedQueue[A] =
      checkf(xs.copy(front = xs.front.tail))
  }
}
