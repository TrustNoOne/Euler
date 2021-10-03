package euler
package til70

object Euler70 extends EulerProblem {

  override def result() = {
    val res = (2 until 10000000).view map { n => (n, φ(n)) } filter { case (n, φn) => isPermutation(n, φn) } minBy {
      case (n, φn) => n.toDouble / φn
    }

    res._1
  }

  // good for n < 10000000
  lazy val φs = euler.totient.Totient(10000000)
  def φ(n: Int): Int = φs(n)

  def isPermutation(x: Int, y: Int): Boolean = {
    x.toString.sorted == y.toString.sorted
  }

}
