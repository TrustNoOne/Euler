package euler
package til50

object Euler49 extends EulerProblem {
  val fourDigitPrimes = Range(1001, 9999, 2).filter(isPrime)

  override def result = {
    val primesWithAtLeastThreePrimePermutations =
      fourDigitPrimes.groupBy(toDigits(_).sorted).values.filter(_.size >= 3)
    /* find all the "distances" between the primes in the set, and filter only
     * the sets that have three primes with the same distance (difference)
     */

    primesWithAtLeastThreePrimePermutations.flatMap { xs => // unreadable?
      xs.combinations(2)
        .toList
        .groupBy(x => x(1) - x(0)) //group by difference
        .values
        .map(_.flatten.toSet)
        .filter(_.size == 3)
        .map(_.mkString)
    }
  }
}
