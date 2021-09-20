package euler
package til50

object Euler50 extends EulerProblem {
  val primes = 2 +: Range(3, 5000, 2).filter(isPrime)

  override def result = {
    val primeSumSeqs =
      (2 to primes.size).view.flatMap(primes.sliding(_)).filter { xs =>
        val sum = xs.sum;
        sum.isPrime && sum < 1000000
      }
    primeSumSeqs.maxBy(_.size).sum
  }
}
