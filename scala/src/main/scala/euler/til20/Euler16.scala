package euler
package til20

object Euler16 extends EulerProblem {
  def sumDigits(n: BigInt, acc: BigInt = BigInt(0)): Long = {
    if (n == 0) acc.toLong
    else sumDigits(n / 10, acc + n % 10)
  }

  override def result() = sumDigits(BigInt(1) << 1000)
}
