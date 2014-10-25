package euler
package til70

import euler._
import contfract._

object Euler66 extends EulerProblem {

  override def result = {
    val res = (2 to 1000) filterNot isPerfectSquare map { d => (d, solveDiophantine(d).get._1) }
    res.maxBy(_._2)._1
  }

  // The fundamental solution is the first convergent that is also a root for the equation
  // http://en.wikipedia.org/wiki/Pell%27s_equation
  def solveDiophantine(n: Int) = {
    val convIt = new ConvergentsIterator(Iterator.continually(1), new SqrContinuedFractionIterator(n))
    convIt find { case (x, y) => x * x - n * y * y == 1 }
  }

}

