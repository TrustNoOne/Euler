package euler
package til80

object Euler78 extends EulerProblem {

  override def result() = {
    val p = Array.ofDim[BigInt](100000)
    val genPentagonals = Euler76.genPentagonalIterator.to(LazyList)

    p(0) = 1

    def loop(n: Int): Int = {
      if (p(n - 1) % 1000000 == 0) {
        n - 1
      } else {
        val pents = genPentagonals takeWhile (_ <= n)
        val nextP = pents.foldLeft((BigInt(0), 0)) { case ((res, i), k) =>
          val sign = if (i / 2 % 2 == 0) 1 else -1
          (res + sign * p(n - k), i + 1)
        }

        p(n) = nextP._1
        loop(n + 1)
      }
    }

    loop(1)

  }

}
