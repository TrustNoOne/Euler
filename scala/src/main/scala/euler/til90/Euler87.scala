package euler
package til90

object Euler87 extends EulerProblem {
  import math._

  override def result = {
    val max = 50000000
    val maxSquareRt = pow(max.toDouble, 1.0 / 2)
    val maxCubeRt = pow(max.toDouble, 1.0 / 3)
    val maxFouthRt = pow(max.toDouble, 1.0 / 4)
    val found = Array.ofDim[Int](max + 1)
    val primes = euler.primes.takeWhile(_ < maxSquareRt).toVector

    for {
      p1 <- primes.iterator
      p2 <- primes.iterator if p2 < maxCubeRt
      p3 <- primes.iterator if p3 < maxFouthRt
      n = p1 * p1 + p2 * p2 * p2 + p3 * p3 * p3 * p3 if n < max
    } { found(n) = 1 }

    found.sum
  }

}
