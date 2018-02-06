package tests

import pfds.DequeAlg
import pfds.bq.BatchedDeque

object BDTests {
  def main(args: Array[String]): Unit = {
    val alg = DequeAlg[BatchedDeque]
    var d = (1 to 10 by 2).foldLeft(alg.empty[Int]) { (acc, i) =>
      alg.cons(i)(acc)
    }
    println(d)
    d = (0 to 9 by 2).foldLeft(d) { (acc, i) =>
      alg.snoc(acc)(i)
    }
    println(d)
    import DequeAlg.ops._
    println(d.head)
    println(d.tail)
    println(d.init)
    println(d.last)
  }
}
