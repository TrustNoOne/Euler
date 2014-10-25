package euler
package til50

object Euler44 extends EulerProblem {
  val limit = 10000

  val pentagonals = Array(1 to limit: _*) map { n => n * (3 * n - 1) / 2 }

  def isPentagonal2(n: Int) = {
    java.util.Arrays.binarySearch(pentagonals, n) >= 0
    //        math.sqrt(24 * n + 1) % 6 == 5
  }

  override def result = {
    for {
      n <- (0 until limit).par
      m <- (n + 1 until limit)
      if (isPentagonal2(pentagonals(m) + pentagonals(n)))
      if (isPentagonal2(pentagonals(m) - pentagonals(n)))
    } yield pentagonals(m) - pentagonals(n)
  }
}