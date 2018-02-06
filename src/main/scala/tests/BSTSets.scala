package tests

import pfds.bst.BinarySearchTree

object BSTSets {
  def main(args: Array[String]): Unit = {
    import pfds.SetAlg.ops._

    val set = BinarySearchTree(1 to 20: _*)
    println(set)
    println(set.member(3))
    println(set.member(21))
    println(set.insert(21))
  }
}
