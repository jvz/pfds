package tests

import pfds.HeapAlg
import pfds.HeapAlg.ops._
import pfds.sh.SplayHeap

object SHTests {
  def main(args: Array[String]): Unit = {
    val alg = HeapAlg[SplayHeap]
    val heap = (1 to 100).foldLeft(alg.empty[Int])(_ :+ _)
    println(heap)
  }
}
