package euler
package til60

object Euler52 extends EulerProblem {
  override def result = {
    val res = Iterator.from(1) dropWhile { x =>
      val sortedDigits = toDigits(x).sorted
      (2 to 6).exists { n => sortedDigits != toDigits(n * x).sorted }
    }
    res.next()
  }
}