package euler
package til10

object Euler10 extends EulerProblem {
  override def result = 2L to 2000000L filter isPrime reduceLeft (_ + _)
}


