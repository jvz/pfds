package pfds

import simulacrum.{op, typeclass}

@typeclass trait QueueAlg[Q[_]] {
  def empty[A]: Q[A]

  def isEmpty[A](xs: Q[A]): Boolean

  @op(":+", alias = true) def snoc[A](xs: Q[A])(x: A): Q[A]

  def head[A](xs: Q[A]): A

  def tail[A](xs: Q[A]): Q[A]
}
