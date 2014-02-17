package euler
package til70

import euler.til20.Euler18

object Euler69 extends EulerProblem {

  override def result = {
    // a little slow
    val res = (2 to 1000000).view.par map { n => (n, n.toDouble / phi(n)) }
    res.maxBy(_._2)._1
  }

  def phi(n: Int): Int = {
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

