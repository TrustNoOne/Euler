package euler
package til30

object Euler23 extends EulerProblem {
  import Euler21.divisors
  override def result = {
    val abundants = (1 to 28123).filter(i => divisors(i).sum > i)
    val sums = collection.mutable.Set.empty[Int]
    for (x <- abundants; y <- abundants) if (x + y <= 28123) sums += x + y

    (1 to 28123).filterNot(sums.contains).sum
  }
}
