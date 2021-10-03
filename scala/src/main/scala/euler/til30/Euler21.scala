package euler
package til30

object Euler21 extends EulerProblem {
  def divisors(n: Int) = (1 to n / 2) filter { n % _ == 0 }

  override def result() = {
    val sumOfDivisors = (2 to 10000) map { divisors(_).sum }
    val amicable = sumOfDivisors.zipWithIndex
      .filter { case (v, i) =>
        v > 1 && v <= 10000 && v != i + 2 && i + 2 == sumOfDivisors(v - 2)
      }
      .map(_._1)
    amicable.sum
  }
}
