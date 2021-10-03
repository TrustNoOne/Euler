package euler
package til50

object Euler46 extends EulerProblem {
  val primes = LazyList.from(3, 2).filter(isPrime) // 2 is not needed

  val oddComposites = LazyList.from(9, 2).filterNot(isPrime)

  def isGoldbach(n: Int) = {
    primes
      .takeWhile(n - _ >= 2)
      .map(p => (n - p) / 2)
      .filter(isPerfectSquare)
      .size > 0
  }

  override def result() = oddComposites.dropWhile(isGoldbach).head
}
