package euler
package til60

object Euler56 extends EulerProblem {

  override def result = {
    val pows = for {
      a <- 1 to 100
      b <- 1 to 100
    } yield BigInt(a).pow(b).toString.map(_ - 48).sum

    pows.max
  }
}
