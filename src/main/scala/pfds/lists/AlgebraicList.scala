package pfds.lists

import pfds.ListAlg

sealed trait AlgebraicList[+A]

case object EmptyCell extends AlgebraicList[Nothing]

case class ConsCell[+A](head: A, tail: AlgebraicList[A]) extends AlgebraicList[A]

object AlgebraicList {
  def empty[A]: AlgebraicList[A] = EmptyCell

  def apply[A](xs: A*): AlgebraicList[A] = xs.foldRight(empty[A])(ConsCell(_, _))

  implicit val instance: ListAlg[AlgebraicList] = new ListAlg[AlgebraicList] {
    override def empty[A]: AlgebraicList[A] = EmptyCell

    override def isEmpty[A](list: AlgebraicList[A]): Boolean = list == EmptyCell

    override def cons[A](hd: A, tl: AlgebraicList[A]): AlgebraicList[A] = ConsCell(hd, tl)

    override def head[A](list: AlgebraicList[A]): A = list match {
      case EmptyCell => throw new NoSuchElementException
      case ConsCell(head, _) => head
    }

    override def tail[A](list: AlgebraicList[A]): AlgebraicList[A] = list match {
      case EmptyCell => throw new UnsupportedOperationException
      case ConsCell(_, tail) => tail
    }
  }
}
