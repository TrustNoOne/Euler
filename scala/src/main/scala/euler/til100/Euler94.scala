package euler
package til100

object Euler94 extends EulerProblem {

  override def result() = {
    LongIterator
      .from(3, 2)
      .take(166666666)
      .flatMap { x =>
        // odd sides 2*m +1
        val m = (x - 1) / 2
        // even sides 2*n
        val n1 = (x + 1) / 2
        val n2 = (x - 1) / 2
        val p1 = 3 * x + 1
        val p2 = 3 * x - 1

        // Heron's formula - semiperimeter = 2m+n+1
        // area = sqrt((2m+n+1)*(2m-n+1)*n*n) = n*sqrt((2m+1)^2-n^2)
        Seq((p1, (2 * m + 1) * (2 * m + 1) - n1 * n1), (p2, (2 * m + 1) * (2 * m + 1) - n2 * n2))

      }
      .filter {
        // double check perfect square, isPerfectSquare has false positives because of fp precision with big numbers
        x => isPerfectSquare(x._2) && isPerfectSquare2(x._2)
      }
      .map(_._1)
      .sum
  }

  def isPerfectSquare2(x: Long) =
    primeFactors(x).groupBy(identity).forall(_._2.size % 2 == 0)

}
