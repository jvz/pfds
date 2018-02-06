package pfds

import simulacrum.{op, typeclass}

@typeclass trait StreamAlg[S[_]] {
  @op("++", alias = true) def append[A](left: S[A])(right: S[A]): S[A]

  def take[A](stream: S[A])(n: Int): S[A]

  def drop[A](stream: S[A])(n: Int): S[A]

  def reverse[A](stream: S[A]): S[A]

  def fold[A, B](stream: S[A])(z: B)(op: (B, A) => B): B

  // added analogous to lists; standard ML doesn't support this as well
  def empty[A]: S[A]

  def isEmpty[A](stream: S[A]): Boolean

  @op("#::", alias = true) def cons[A](hd: A, tl: => S[A]): S[A]

  def head[A](stream: S[A]): A

  def tail[A](stream: S[A]): S[A]
}
