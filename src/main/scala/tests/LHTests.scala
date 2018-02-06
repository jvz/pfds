package tests

import pfds.HeapAlg
import pfds.lh.LeftistHeap

object LHTests {
  def main(args: Array[String]): Unit = {
    import pfds.HeapAlg.ops._

    val heap = (1 to 10).foldLeft(HeapAlg[LeftistHeap].empty[Int]) { (tree, i) =>
      tree.insert(i)
    }
    println(heap)
    println(heap.insert(0))
    println(heap.findMin)
  }
}
