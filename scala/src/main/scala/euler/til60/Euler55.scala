package euler
package til60

object Euler55 extends EulerProblem {

  private def isLychrel(n: Int) = {
    def loop(n: BigInt, cnt: Int): Boolean =
      if (cnt == 50) true
      else {
        val next = n + BigInt(n.toString.reverse);
        if (isPalindrome(next.toString)) false
        else loop(next, cnt + 1)
      }

    loop(n, 0)
  }

  override def result() = {
    1 to 10000 count isLychrel
  }
}
