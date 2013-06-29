package euler
package til40

object Euler32 extends EulerProblem {
  override def result = {
    val prods = for (a <- Range(1, 100).view; b <- Range(1, 10000).view if a * b < 9876)
      yield (a * b, "" + a + b + (a * b))
    prods.filter(x => x._2.length == 9 && !x._2.contains('0') && x._2.toSet.size == 9).map(_._1).toSet.sum
  }
}
