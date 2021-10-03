package euler
package til20

object Euler20 extends EulerProblem {
  def fact(n: Int, acc: BigInt = 1): BigInt =
    if (n <= 1) acc else fact(n - 1, acc * n)

  override def result() = Euler16.sumDigits(fact(100))
}
