package euler
package til20

object Euler12 extends EulerProblem {
  def numDivisors(n: Long, curr: Long = 2, cnt: Int = 0, acc: Int = 1): Int = {
    if (curr > n) acc * (cnt + 1)
    else if (n % curr == 0) numDivisors(n / curr, curr, cnt + 1, acc)
    else numDivisors(n, curr + 1, 0, acc * (cnt + 1))
  }

  override def result() = {
    var i = 1L
    while (numDivisors(i * (i + 1) / 2) <= 500) i += 1
    i * (i + 1) / 2
  }
}
