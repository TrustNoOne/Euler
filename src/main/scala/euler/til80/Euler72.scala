package euler
package til80

object Euler72 extends EulerProblem {

  override def result = {
    //sum of all coprimes to each possible denominator
    (2 to 1000000).foldLeft(0L)((t, x) => t + φ(x))
  }

  lazy val φs = euler.totient.Totient(1000001)
  def φ(n: Int): Int = φs(n)

}

