package pfds

import simulacrum.typeclass

@typeclass trait SetAlg[S[_]] {
  def empty[A]: S[A]

  def insert[A](set: S[A])(elem: A)(implicit ordering: Ordering[A]): S[A]

  def member[A](set: S[A])(elem: A)(implicit ordering: Ordering[A]): Boolean
}
