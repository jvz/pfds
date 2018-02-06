package pfds.bst

import pfds.SetAlg

sealed trait BinarySearchTree[+A]

case object Leaf extends BinarySearchTree[Nothing]

case class Node[+A](value: A, left: BinarySearchTree[A], right: BinarySearchTree[A]) extends BinarySearchTree[A]

object BinarySearchTree {
  implicit val instance: SetAlg[BinarySearchTree] = new SetAlg[BinarySearchTree] {
    override def empty[A]: BinarySearchTree[A] = Leaf

    override def insert[A](set: BinarySearchTree[A])(elem: A)(implicit ordering: Ordering[A]): BinarySearchTree[A] = set match {
      case Leaf => Node(elem, empty, empty)
      case t@Node(value, left, right) =>
        import ordering._
        if (elem < value) Node(value, insert(left)(elem), right)
        else if (elem > value) Node(value, left, insert(right)(elem))
        else t
    }

    override def member[A](set: BinarySearchTree[A])(elem: A)(implicit ordering: Ordering[A]): Boolean = set match {
      case Leaf => false
      case Node(value, left, right) =>
        import ordering._
        if (elem < value) member(left)(elem)
        else if (elem > value) member(right)(elem)
        else true
    }
  }

  def apply[A: Ordering](xs: A*): BinarySearchTree[A] =
    xs.foldLeft(instance.empty[A]) { (tree, x) => instance.insert(tree)(x) }
}
