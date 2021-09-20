package euler
package til60

object Euler51 extends EulerProblem {

  /**
    * primePermutations(123, 2) returns all prime permutations of 123xx with xx == 00,11,..,99
    */
  def primePermutations(n: Int, addedDigitCnt: Int) = {
    val digits = toDigits(n) ++ List.fill(addedDigitCnt)(-1)
    digits.permutations.map { x =>
      (0 to 9).map { d =>
        x.map { case -1 => d; case a => a }
      } map fromDigits filter isPrime
    } filterNot { _.isEmpty }
  }

  override def result = {
    val res = LazyList.from(100) flatMap { primePermutations(_, 3) } dropWhile (
        xs => xs.size < 8 || xs.head < 100000)
    res.head.head
  }
}
