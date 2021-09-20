package euler
package til100

object Euler100 extends EulerProblem {

  val D = 2
  val N = 2
  val MAX = BigInt(1000000000000L)

  /**
    * B/T*(B-1)/(T-1) = 1/2
    * BK² - BK - T² + T = 0
    * x = 4B + 2, y = -2T + 1
    * x² - Dy² -N = 0
    * x² - 2y² -2 = 0 -> D=2, N=2
    * minimal positive solution for x - 2y² = 1 -> (t,u) = (3,2)
    * fundamental solution of x² - 2y² -2 = 0 -> (r,s) = (2,1)
    *
    * If r, s is a solution to x² − Dy² = N, and t, u is any solution to x² − Dy² = 1,
    * then x = rt + suD, y = ru + st, is also a solution to x² − Dy² = N.
    */
  override def result = {
    def loop(r: BigInt, s: BigInt, t: BigInt, u: BigInt): (BigInt, BigInt) = {
      val B = (s + 1) / 2
      if (B > MAX) (r, s)
      else loop(r * t + s * u * D, r * u + s * t, t, u)
    }

    val (x, _) = loop(2, 1, 3, 2)
    (x - 2) / 4 + 1
  }

}
