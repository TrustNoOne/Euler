package euler
package til20

object Euler15 extends EulerProblem {
  def binomial(n: Long, k: Long): Long = {
    ((n - k + 1L to n).foldLeft(BigInt(1))(_ * _) / (2L to k).foldLeft(BigInt(1))(_ * _)).toLong
  }

  override def result = binomial(40, 20)
}

