package pfds

import simulacrum.{op, typeclass}

@typeclass trait ListAlg[L[+ _]] {
  def empty[A]: L[A]

  def isEmpty[A](list: L[A]): Boolean

  @op("::", alias = true)
  def cons[A](hd: A, tl: L[A]): L[A]

  def head[A](list: L[A]): A

  def tail[A](list: L[A]): L[A]
}
