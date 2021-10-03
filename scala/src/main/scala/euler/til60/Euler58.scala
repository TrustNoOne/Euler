package euler
package til60

object Euler58 extends EulerProblem {

  override def result() = {
    /*
     * 1 iteration = 1 spiral cycle
     * center elem: 1
     * upper-right diag: 2*1+1 4*3+1 6*5+1... = i * (i - 1) + 1
     * upper-left  diag: 2*2+1 4*4+1 6*6+1... = i * i + 1
     * lower-left  diag: 2*3+1 4*5+1 6*7+1... = i * (i + 1) +1
     * lower-right diag are not primes
     */
    val diagElems = Iterator.from(2, 2).scanLeft((0, 1)) { (totPrimes, i) =>
      val diagElemsCount = 2 * i + 1
      val newPrimes = Seq(i * (i - 1) + 1, i * i + 1, i * (i + 1) + 1) count isPrime
      (totPrimes._1 + newPrimes, diagElemsCount)
    }

    //first is 0
    val diagElemsCountResult =
      diagElems.drop(1).dropWhile(x => x._1.toDouble / x._2 >= 0.1).next()._2
    (diagElemsCountResult - 1) / 2 + 1 // side length
  }

}
