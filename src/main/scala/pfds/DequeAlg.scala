package pfds

import simulacrum.typeclass

@typeclass trait DequeAlg[D[_]] {
  def empty[A]: D[A]

  def isEmpty[A](xs: D[A]): Boolean

  def cons[A](head: A)(tail: D[A]): D[A]

  def head[A](xs: D[A]): A

  def tail[A](xs: D[A]): D[A]

  def snoc[A](init: D[A])(last: A): D[A]

  def init[A](xs: D[A]): D[A]

  def last[A](xs: D[A]): A
}
