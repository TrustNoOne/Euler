package euler
package til80

import scala.annotation.tailrec

object Euler76 extends EulerProblem {

  override def result = {
    val p = Array.ofDim[Int](101)
    val genPentagonals = genPentagonalIterator.toStream

    p(0) = 1
    for {
      n <- 1 to 100
      pents = genPentagonals takeWhile (n - _ >= 0)
    } {
      val nextP = pents.foldLeft((0, 0)) {
        case ((res, i), k) =>
          val sign = if (i / 2 % 2 == 0) 1 else -1
          (res + sign * p(n - k), i + 1)
      }

      p(n) = nextP._1
    }

    p(100) - 1
  }

  def genPentagonalIterator = new Iterator[Int] {
    val hasNext = true
    var k = 0

    def next = {
      if (k > 0) k = -k else k = -k + 1
      k * (3 * k - 1) / 2
    }
  }

}

