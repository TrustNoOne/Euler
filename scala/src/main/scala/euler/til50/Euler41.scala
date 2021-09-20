package euler
package til50

object Euler41 extends EulerProblem {
  override def result = {
    val digits = List(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)
    val pandigitalPrimes = (1 to 9) flatMap { n =>
      digits.take(n).permutations.filter(fromDigits(_).isPrime).map(fromDigits)
    }
    pandigitalPrimes.sorted.last
  }
}
