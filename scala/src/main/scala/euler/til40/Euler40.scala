package euler
package til40

object Euler40 extends EulerProblem {
  override def result = {
    val champernowne = Iterator.from(1).flatMap(toDigits(_).reverse)
    champernowne
      .next() * champernowne.drop(8).next() * champernowne.drop(89).next() *
      champernowne.drop(899).next() * champernowne
      .drop(8999)
      .next() * champernowne.drop(89999).next() *
      champernowne.drop(899999).next()
  }
}
