package euler
package til70

object Euler69 extends EulerProblem {

  override def result = {
    val res = (2 to 1000000).view.par map { n => (n, n.toDouble / φ(n)) }
    res.maxBy(_._2)._1
  }

  // good for n < 1000001
  lazy val φs = euler.totient.Totient(1000001)
  def φ(n: Int): Int = φs(n)

  def φslow(n: Int): Int = {
    val primes = euler.primes.iterator
    @annotation.tailrec
    def loop(n: Int, acc: Double, p: Int, first: Boolean): Double = n match {
      case 1 => acc
      case n if n % p == 0 =>
        val newAcc = if (first) acc * (p - 1) / p else acc
        loop(n / p, newAcc, p, false)
      case _ => loop(n, acc, primes.next, true)
    }

    loop(n, n.toDouble, primes.next, true).toInt
  }

}

