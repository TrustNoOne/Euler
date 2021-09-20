package euler
package til30

object Euler26 extends EulerProblem {
  def decimals(n: Int, d: Int): LazyList[(Int, Int)] = {
    if (n == 0) LazyList.empty
    else ((n % d * 10 / d), n % d) #:: decimals(n % d * 10, d)
  }

  def repeatingDecimals(n: Int, d: Int): List[Int] = {
    var remainders: List[Int] = Nil
    var remainIdx: Int = 0
    val repeating = decimals(n, d) takeWhile { x =>
      remainIdx = remainders.indexOf(x._2)
      remainders ::= x._2
      remainIdx < 0
    }
    repeating.map(_._1).toList.takeRight(remainIdx + 1)
  }

  override def result =
    (2 until 1000).map(repeatingDecimals(1, _).size).zipWithIndex.max._2 + 2

}
