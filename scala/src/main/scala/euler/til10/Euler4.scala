package euler
package til10

object Euler4 extends EulerProblem {
  def isPalindrome(n: Int) = (n.toString, n.toString.reverse).zipped forall (_ == _)

  override def result = {
    var max = (0, 0, 0)
    for (i <- 100 to 999; j <- i to 999) {
      val n = i * j
      if (isPalindrome(n) && n > max._1) max = (n, i, j)
    }
    max
  }
}