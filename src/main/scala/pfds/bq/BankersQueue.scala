package pfds.bq

import pfds.QueueAlg

case class BankersQueue[A](lenf: Int, f: Stream[A], lenr: Int, r: Stream[A])

object BankersQueue {

  def apply[A](xs: A*): BankersQueue[A] =
    xs.foldLeft(empty[A])(instance.snoc(_)(_))

  def empty[A]: BankersQueue[A] =
    BankersQueue(0, Stream.empty, 0, Stream.empty)

  implicit val instance: QueueAlg[BankersQueue] = new QueueAlg[BankersQueue] {
    override def empty[A]: BankersQueue[A] =
      BankersQueue.empty

    override def isEmpty[A](xs: BankersQueue[A]): Boolean =
      xs.lenf == 0

    // maintain the invariant lenf >= lenr
    def check[A](q: BankersQueue[A]): BankersQueue[A] =
      if (q.lenr <= q.lenf) q else BankersQueue(q.lenf + q.lenr, q.f #::: q.r.reverse, 0, Stream.empty)

    override def snoc[A](xs: BankersQueue[A])(x: A): BankersQueue[A] =
      check(xs.copy(lenr = xs.lenr + 1, r = x #:: xs.r))

    override def head[A](xs: BankersQueue[A]): A = xs.f match {
      case x #:: _ => x
      case _ => throw new NoSuchElementException
    }

    override def tail[A](xs: BankersQueue[A]): BankersQueue[A] = xs.f match {
      case _ #:: f => check(xs.copy(lenf = xs.lenf - 1, f = f))
      case _ => throw new UnsupportedOperationException
    }
  }
}
