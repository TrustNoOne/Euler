package euler
package til10

object Euler7 extends EulerProblem {
  def buildPrime(count: Int, n: Int): Int = {
    if (isPrime(n) && count == 10001) n
    else if (isPrime(n)) buildPrime(count + 1, n + 1)
    else buildPrime(count, n + 1)
  }

  override def result = {
    buildPrime(1, 2)
  }
}