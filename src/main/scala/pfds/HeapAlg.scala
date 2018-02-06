package pfds

import simulacrum.{op, typeclass}

@typeclass trait HeapAlg[H[_]] {
  def empty[A](implicit ordering: Ordering[A]): H[A]

  def isEmpty[A](heap: H[A])(implicit ordering: Ordering[A]): Boolean

  @op(":+", alias = true)
  def insert[A](heap: H[A])(elem: A)(implicit ordering: Ordering[A]): H[A]

  @op("++", alias = true)
  def merge[A](left: H[A])(right: H[A])(implicit ordering: Ordering[A]): H[A]

  def findMin[A](heap: H[A])(implicit ordering: Ordering[A]): Option[A]

  def deleteMin[A](heap: H[A])(implicit ordering: Ordering[A]): H[A]
}
