package euler.totient

class Totient(max: Int) {
  val totients = Array.fill(max)(1)
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
    i <- Range(2, max) if totients(i) == 1
    j <- Range(i, max, i)
  } {
    totients(j) *= φp(j, i)
  }

  def apply(n: Int) = totients(n)
}

object Totient {
  def apply(max: Int) = new Totient(max)
}
