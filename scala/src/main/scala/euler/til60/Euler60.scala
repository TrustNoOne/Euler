package euler
package til60

/**
  * This is too slow! we have to load a prime table
  */
object Euler60 extends EulerProblem {

  def longFrom(start: Long, step: Long): LazyList[Long] =
    start #:: longFrom(start + step, step)

  val numOfPrimes = 2500 // magic number
  val primes = longFrom(3, 2).filter(euler.isPrime).take(numOfPrimes).toArray

  val cached = collection.mutable.HashMap[Long, Boolean]()
  def isPrime(n: Long) = cached.getOrElseUpdate(n, euler.isPrime(n))

  override def result = {
    val res = for {
      i1 <- (0 to primes.size - 5)
      i2 <- (i1 to primes.size - 4) if isConcatPrime(primes(i2), primes(i1))
      i3 <- (i2 to primes.size - 3)
      if isConcatPrime(primes(i3), primes(i1)) && isConcatPrime(primes(i3),
                                                                primes(i2))
      i4 <- (i3 to primes.size - 2)
      if isConcatPrime(primes(i4), primes(i1)) && isConcatPrime(
        primes(i4),
        primes(i2)) && isConcatPrime(
        primes(i4),
        primes(i3),
      )
      i5 <- (i4 to primes.size - 1)
      if isConcatPrime(primes(i5), primes(i1)) && isConcatPrime(
        primes(i5),
        primes(i2)) && isConcatPrime(
        primes(i5),
        primes(i3),
      ) && isConcatPrime(primes(i5), primes(i4))
    } yield (primes(i1), primes(i2), primes(i3), primes(i4), primes(i5))

    res.toList
  }

  def isConcatPrime(x: Long, y: Long): Boolean = {
    import math._
    isPrime(pow(10, floor(log10(y.toDouble)) + 1).toLong * x + y) && isPrime(
      pow(10, floor(log10(x.toDouble)) + 1).toLong * y + x,
    )
  }

}
