package euler
package til40

object Euler37 extends EulerProblem {
  override def result = {
    Stream.from(9, 2).filter { n =>
      val lr = toDigits(n).scanLeft((1, 0))((s, x) => (s._1 * 10, s._2 + s._1 * x)).map(_._2).drop(1)
      val rl = toDigits(n).reverse.scanLeft(0)((s, x) => s * 10 + x).drop(1)
      (lr forall isPrime) && (rl forall isPrime)
    }.take(11).sum
  }
}