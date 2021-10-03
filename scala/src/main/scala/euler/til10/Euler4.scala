package euler
package til10

object Euler4 extends EulerProblem {
  def isPalindrome(n: Int) =
    n.toString.lazyZip(n.toString.reverse) forall (_ == _)

  override def result() = {
    val palindromes = for {
      i <- 100 to 999
      j <- i to 999
      n = i * j
      if isPalindrome(n)
    } yield n

    palindromes.max
  }
}
