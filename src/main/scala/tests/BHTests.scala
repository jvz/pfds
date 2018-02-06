package tests

import pfds.bh.BinomialHeap

object BHTests {
  def main(args: Array[String]): Unit = {
    import pfds.HeapAlg.ops._

    val heap = BinomialHeap(1 to 10: _*)

    println(heap)
    println(heap.findMin)
    println(heap.deleteMin)
  }
}
