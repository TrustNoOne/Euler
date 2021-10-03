package euler
package til30

object Euler27 extends EulerProblem {

  override def result() = {
    val numPrimes = for {
      a <- Range(-999, 999).view
      b <- Range(-999, 999).view
    } yield (LazyList.from(0).map(n => n * n + a * n + b).takeWhile(isPrime).size, a, b)
    val maxNumPrimes = numPrimes.maxBy(_._1)
    maxNumPrimes._2 * maxNumPrimes._3
  }
}
