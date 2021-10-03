package euler
package til50

object Euler43 extends EulerProblem {
  def subStringDivCheck(xs: Seq[Long]) = {
    fromDigits(xs.slice(6, 9)) % 2 == 0 &&
    fromDigits(xs.slice(5, 8)) % 3 == 0 &&
    fromDigits(xs.slice(4, 7)) % 5 == 0 &&
    fromDigits(xs.slice(3, 6)) % 7 == 0 &&
    fromDigits(xs.slice(2, 5)) % 11 == 0 &&
    fromDigits(xs.slice(1, 4)) % 13 == 0 &&
    fromDigits(xs.slice(0, 3)) % 17 == 0
  }

  override def result() = {
    List(0L, 1, 2, 3, 4, 5, 6, 7, 8, 9).permutations.filter(subStringDivCheck).map(fromDigits).sum
  }
}
