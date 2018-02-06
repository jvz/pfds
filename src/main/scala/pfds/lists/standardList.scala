package pfds.lists

import pfds.ListAlg

object standardList {
  implicit val instance: ListAlg[List] = new ListAlg[List] {
    override def empty[A]: List[A] = Nil

    override def isEmpty[A](list: List[A]): Boolean = list.isEmpty

    override def cons[A](hd: A, tl: List[A]): List[A] = hd :: tl

    override def head[A](list: List[A]): A = list.head

    override def tail[A](list: List[A]): List[A] = list.tail
  }
}
