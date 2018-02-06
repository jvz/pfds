package tests

import pfds.StreamAlg
import pfds.streams.StreamCell

object StreamTests {

  // exercise 4.2
  implicit class StreamSortOps[S[_], A](val xs: S[A]) extends AnyVal {
    def insert(x: A)(implicit alg: StreamAlg[S], ordering: Ordering[A]): S[A] = {
      import alg._
      import ordering._
      if (isEmpty(xs)) cons(x, empty)
      else if (x <= head(xs)) cons(x, xs)
      else cons(head(xs), tail(xs).insert(x))
    }

    def sorted(implicit alg: StreamAlg[S], ordering: Ordering[A]): S[A] = {
      import alg._
      fold(xs)(empty[A])(_.insert(_))
    }

    def mkString(sep: String)(implicit alg: StreamAlg[S]): String = {
      val sb = alg.fold(xs)(new StringBuilder)(_ append _ append sep)
      sb.length -= sep.length
      sb.toString()
    }

    def foreach(op: A => Unit)(implicit alg: StreamAlg[S]): Unit =
      alg.fold(xs)(Unit: Unit)((_, x) => op(x))
  }

  def main(args: Array[String]): Unit = {
    import StreamAlg.ops._
    val xs = StreamCell.range(1, 10)
    println(xs.mkString(", "))
    println(xs.insert(0).mkString(","))
    val ys = StreamCell.randomInts(42).take(10)
    println(ys.mkString(","))
    println(ys.sorted.mkString(","))
  }
}
