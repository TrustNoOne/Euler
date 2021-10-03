package euler
package til30

object Euler25 extends EulerProblem {
  def fibWithDigits(numDigits: Int) = {
    def fibWithDigitsTr(curr: BigInt, prev: BigInt, idx: BigInt): BigInt =
      curr match {
        case x if x.toString.length >= numDigits => idx + 1
        case _ => fibWithDigitsTr(curr + prev, curr, idx + 1)
      }
    fibWithDigitsTr(1, 1, 1)
  }

  override def result() = fibWithDigits(1000)
}
