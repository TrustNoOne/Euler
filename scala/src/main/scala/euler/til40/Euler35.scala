package euler
package til40

object Euler35 extends EulerProblem {
  def rotations(n: Int) = {
    def rotStream(n: Int): Stream[Int] = {
      val digits = toDigits(n)
      val next = fromDigits(digits.tail :+ digits.head)
      next #:: rotStream(next)
    }
    n #:: (rotStream(n) takeWhile (_ != n))
  }

  override def result = Range(3, 1000000, 2).filter(rotations(_) forall isPrime).size + 1 //2
}