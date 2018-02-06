package tests

import pfds.rb.RedBlackTree

object RBTSets {
  def main(args: Array[String]): Unit = {
    import pfds.SetAlg.ops._

    val set = RedBlackTree.fromOrdList((1 to 10).toList)
    println(set)
    println(set.member(3))
    println(set.member(11))
    println(set.insert(1))
    println(set.insert(11))
  }
}
