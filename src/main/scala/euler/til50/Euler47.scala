package euler
package til50

object Euler47 extends EulerProblem {
  val primes = 2 #:: Stream.from(3, 2).filter(isPrime)

  def factorCount(n: Int) = primes.takeWhile(_ <= n / 2).count(n % _ == 0)

  def factorCount2(n: Int) = { // 10 times faster
    def loop(n: Int, cnt: Int, primes: Stream[Int]): Int = n match {
      case x if primes.head > x => cnt
      case x if x % primes.head == 0 => loop(x / primes.head, cnt + 1, primes.tail)
      case _ => loop(n, cnt, primes.tail)
    }
    loop(n, 0, primes)
  }

  override def result = {
    val res = Iterator.from(2 * 3 * 5 * 7).dropWhile { n =>
      (n to n + 3).exists(factorCount2(_) != 4)
    }
    res.next
  }
}