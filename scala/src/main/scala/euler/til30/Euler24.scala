package euler
package til30

object Euler24 extends EulerProblem {
  def res(_idx: Int, numElems: Int) = {
    import scala.collection.mutable.ListBuffer
    val idx = _idx - 1 /*1-based*/
    var remain: Int = idx
    var elems = ListBuffer.range(0, numElems)
    (0 until numElems) map { i =>
      if (remain == 0) elems.remove(0)
      else {
        val currFact = fact(elems.size - 1).toInt
        val nextDigitIdx = remain / currFact
        remain %= currFact
        elems.remove(nextDigitIdx)
      }
    }
  }

  override def result = res(1000000, 10).mkString
}
