package euler
package til30

object Euler29 extends EulerProblem {
  override def result = {
    val pows = for {
      a <- 2 to 100
      b <- 2 to 100
    } yield math.pow(a.toDouble, b.toDouble)
    Set(pows: _*).size
  }
}
