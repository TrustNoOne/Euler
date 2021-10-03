package euler
package til70

object Euler63 extends EulerProblem {
  override def result() = {
    Iterator.from(1).map(powerfulDigitals).takeWhile(!_.isEmpty).foldLeft(0) { (res, x) => res + x.size }
  }

  def powerfulDigitals(nDigits: Int) = {
    val res = bigIntStream(1) dropWhile { x => x.pow(nDigits).toString.length < nDigits } takeWhile { x =>
      x.pow(nDigits).toString.length == nDigits
    }
    res.toList
  }

}
