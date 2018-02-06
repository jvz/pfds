package pfds.streams

import pfds.StreamAlg

object standardStream {
  implicit val instance: StreamAlg[Stream] = new StreamAlg[Stream] {
    override def append[A](left: Stream[A])(right: Stream[A]): Stream[A] = left #::: right

    override def take[A](stream: Stream[A])(n: Int): Stream[A] = stream.take(n)

    override def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream.drop(n)

    override def reverse[A](stream: Stream[A]): Stream[A] = stream.reverse

    override def fold[A, B](stream: Stream[A])(z: B)(op: (B, A) => B): B = stream.foldLeft(z)(op)

    override def empty[A]: Stream[A] = Stream.empty[A]

    override def isEmpty[A](stream: Stream[A]): Boolean = stream.isEmpty

    override def cons[A](hd: A, tl: => Stream[A]): Stream[A] = hd #:: tl

    override def head[A](stream: Stream[A]): A = stream.head

    override def tail[A](stream: Stream[A]): Stream[A] = stream.tail
  }
}
