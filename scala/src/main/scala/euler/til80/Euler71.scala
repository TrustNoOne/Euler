package euler
package til80

object Euler71 extends EulerProblem {

  override def result() = {
    val target = 3.toDouble / 7

    (8 to 1000000) map { d =>
      (binSearch(d, target), d)
    } filter { // the binary search could produce a non-proper fraction, discard those
      case (n, d) => mcd(n, d) == 1
    } maxBy { case (n, d) => n.toDouble / d }
  }

  /**
   * binary search for a certain denominator "d"
   * to determine the greatest n such that n/d < target
   */
  def binSearch(d: Int, target: Double) = {
    @annotation.tailrec
    def loop(nStart: Int, nEnd: Int): Int = {
      val n = (nEnd + nStart) / 2
      if (n == nStart) nStart
      else if (n.toDouble / d > target) loop(nStart, n)
      else loop(n, nEnd)
    }
    loop(0, d)
  }

  def mcd(a: Int, b: Int): Int = {
    val rem = a % b
    if (rem == 0) b else mcd(b, rem)
  }

}
