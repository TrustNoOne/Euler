package euler
package til70

import euler.til20.Euler18

object Euler69 extends EulerProblem {

  override def result = {
    // a little slow
    val res = (2 to 1000000).view.par map { n => (n, n.toDouble / φ(n)) }
    res.maxBy(_._2)._1
  }

  def totients(n: Int): Array[Int] = {
    val totients = Array.fill(n)(1)
    // finds the totient "contributor" to φ(n) for the divisor of n "p".
    def φp(n: Int, p: Int): Int = {
      @annotation.tailrec
      def loop(n: Int, p: Int, acc: Int): Int = {
        if (n % p == 0) loop(n / p, p, acc * p)
        else acc
      }

      if (n % p != 0) 1 else loop(n / p, p, p - 1)
    }

    for {
      i <- Range(2, n) if totients(i) == 1
      j <- Range(i, n, i)
    } { totients(j) *= φp(j, i) }

    totients
  }

  // good for n < 1000001
  val φs = totients(1000001)
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

