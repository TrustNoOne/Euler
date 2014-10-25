package euler
package til40

object Euler38 extends EulerProblem {
  def isConcProdPandigital(n: Int): Option[Int] = {
    def calcPanRec(i: Int, res: Seq[Int]): Seq[Int] = res match {
      case x if x.length >= 9 => x
      case x => calcPanRec(i + 1, toDigits(n * i) ++: x)
    }

    val digits = calcPanRec(1, Nil)
    val isPandigital = digits.length == 9 && !digits.contains(0) && digits.toSet.size == 9
    if (isPandigital) Some(fromDigits(digits)) else None
  }

  override def result = {
    val pandigits = (1 to 9999).par map isConcProdPandigital filter { _.isDefined } map { _.get }
    pandigits.toList.sorted.last
  }
}