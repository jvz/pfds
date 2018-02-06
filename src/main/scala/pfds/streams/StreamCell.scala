package pfds.streams

import pfds.StreamAlg

import scala.annotation.tailrec
import scala.util.Random

sealed trait StreamCell[+A] {
  def head: A

  def tail: StreamCell[A]
}

case object SNil extends StreamCell[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: StreamCell[Nothing] = throw new UnsupportedOperationException
}

final class SCons[+A](hd: A, tl: => StreamCell[A]) extends StreamCell[A] {
  val head: A = hd
  lazy val tail: StreamCell[A] = tl
}

object SCons {
  def apply[A](hd: A, tl: => StreamCell[A]) = new SCons(hd, tl)

  def unapply[A](xs: SCons[A]): Option[(A, StreamCell[A])] = Some((xs.head, xs.tail))
}

object StreamCell {
  implicit val instance: StreamAlg[StreamCell] = new StreamAlg[StreamCell] {
    override def append[A](left: StreamCell[A])(right: StreamCell[A]): StreamCell[A] = left match {
      case SNil => right
      case SCons(hd, tl) => SCons(hd, append(tl)(right))
    }

    override def take[A](stream: StreamCell[A])(n: Int): StreamCell[A] =
      if (n == 0) SNil else stream match {
        case SNil => SNil
        case SCons(hd, tl) => SCons(hd, take(tl)(n - 1))
      }

    override def drop[A](stream: StreamCell[A])(n: Int): StreamCell[A] =
      if (n == 0) stream else stream match {
        case SNil => SNil
        case SCons(_, tl) => drop(tl)(n - 1)
      }

    override def reverse[A](stream: StreamCell[A]): StreamCell[A] = {
      @tailrec
      def go(orig: StreamCell[A], rev: StreamCell[A]): StreamCell[A] = orig match {
        case SNil => rev
        case SCons(hd, tl) => go(tl, SCons(hd, rev))
      }

      go(stream, SNil)
    }

    override def fold[A, B](stream: StreamCell[A])(z: B)(op: (B, A) => B): B = stream match {
      case SNil => z
      case SCons(hd, tl) => fold(tl)(op(z, hd))(op)
    }

    override def empty[A]: StreamCell[A] = SNil

    override def isEmpty[A](stream: StreamCell[A]): Boolean = stream == SNil

    override def cons[A](hd: A, tl: => StreamCell[A]): StreamCell[A] = SCons(hd, tl)

    override def head[A](stream: StreamCell[A]): A = stream.head

    override def tail[A](stream: StreamCell[A]): StreamCell[A] = stream.tail
  }

  def range[A](start: A, end: A)(implicit num: Numeric[A]): StreamCell[A] = {
    import num._
    if (start <= end) SCons(start, range(start + one, end)) else SNil
  }

  def randomInts(seed: Long = System.currentTimeMillis()): StreamCell[Int] = {
    val random = new Random(seed)
    def next: StreamCell[Int] = SCons(random.nextInt(), next)
    next
  }
}
