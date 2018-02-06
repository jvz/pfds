package pfds.rb

import pfds.SetAlg

sealed trait RedBlackTree[+A] {
  val color: Color
}

case object Leaf extends RedBlackTree[Nothing] {
  override val color: Color = Black
}

case class Node[+A](color: Color, value: A, left: RedBlackTree[A], right: RedBlackTree[A]) extends RedBlackTree[A]

object RedBlackTree {
  implicit val instance: SetAlg[RedBlackTree] = new SetAlg[RedBlackTree] {
    override def empty[A]: RedBlackTree[A] = Leaf

    override def insert[A](set: RedBlackTree[A])(elem: A)(implicit ordering: Ordering[A]): RedBlackTree[A] = {
      def node(x: A, y: A, z: A, a: RedBlackTree[A], b: RedBlackTree[A], c: RedBlackTree[A], d: RedBlackTree[A]): Node[A] =
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

      def balance(color: Color, value: A, left: RedBlackTree[A], right: RedBlackTree[A]): Node[A] =
        if (color == Red) Node(color, value, left, right) else (left, right) match {
          case (Node(Red, y, Node(Red, x, a, b), c), d) =>
            node(x, y, value, a, b, c, d)
          case (Node(Red, x, a, Node(Red, y, b, c)), d) =>
            node(x, y, value, a, b, c, d)
          case (a, Node(Red, z, Node(Red, y, b, c), d)) =>
            node(value, y, z, a, b, c, d)
          case (a, Node(Red, y, b, Node(Red, z, c, d))) =>
            node(value, y, z, a, b, c, d)
          case _ =>
            Node(color, value, left, right)
        }

      def ins(set: RedBlackTree[A]): Node[A] = set match {
        case Leaf => Node(Red, elem, empty, empty)
        case s@Node(color, b, left, right) =>
          import ordering._
          if (elem < b) balance(color, b, ins(left), right)
          else if (elem > b) balance(color, b, left, ins(right))
          else s
      }

      ins(set).copy(color = Black)
    }

    override def member[A](set: RedBlackTree[A])(elem: A)(implicit ordering: Ordering[A]): Boolean = set match {
      case Leaf => false
      case Node(_, b, left, right) =>
        import ordering._
        if (elem < b) member(left)(elem)
        else if (elem > b) member(right)(elem)
        else true
    }
  }

  // exercise 3.9
  def fromOrdList[A: Ordering](xs: List[A]): RedBlackTree[A] =
    xs.foldLeft(instance.empty[A]) { (tree, x) =>
      instance.insert(tree)(x)
    }
}
