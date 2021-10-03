package euler
package til40

object Euler36 extends EulerProblem {
  def toBinaryDigits(n: Int): Seq[Int] =
    if (n < 2) Seq(n) else (n % 2) +: toDigits(n / 2)

  def isPalindrome[T](xs: Seq[T]) =
    (xs zip xs.reverse) take (xs.size / 2) forall (x => x._1 == x._2)

  override def result() = {
    (1 to 1000000).filter { n => isPalindrome(toDigits(n)) && isPalindrome(toBinaryDigits(n)) }.sum
  }
}
