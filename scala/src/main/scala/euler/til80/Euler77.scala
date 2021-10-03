package euler
package til80

object Euler77 extends EulerProblem {

  override def result() = {
    val res = primePartionCnts(10000).zipWithIndex.find(_._1 > 5000).get
    res._2
  }

  def primePartionCnts(max: Int): Array[Int] = {
    val res = Array.fill(max)(0)
    val sopf = sopfs(max)
    for (n <- Range(2, max)) {
      for (j <- Range(1, n)) {
        res(n) += sopf(j) * res(n - j)
      }
      res(n) = (res(n) + sopf(n)) / n
    }
    res
  }

  //sum of prime factors (cached array)
  def sopfs(max: Int): Array[Int] = {
    val sopf = Array.fill(max)(0)

    for {
      i <- Range(2, max) if sopf(i) == 0
      j <- Range(i, max, i)
    } { sopf(j) += i }

    sopf
  }

}
