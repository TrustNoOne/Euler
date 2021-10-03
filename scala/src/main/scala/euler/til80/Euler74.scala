package euler
package til80

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Euler74 extends EulerProblem {

  override def result() = {
    (1 until 1000000).view.par count { chainLen(_) == 60 }
  }

  def chainLen(n: Int) = {
    @tailrec
    def loop(n: Int, xs: Set[Int]): Int = {
      val next = n.toDigits.reverse.map(fact).sum;
      if (xs.contains(next)) xs.size + 1
      else loop(next, xs + next)
    }
    loop(n, Set())
  }

  def fact(n: Int): Int = {
    @tailrec
    def f(n: Int, acc: Int = 1): Int =
      if (n <= 1) acc else f(n - 1, acc * n)
    f(n)
  }
}
