package euler
package til70

import euler._

object Euler64 extends EulerProblem {

  override def result = {
    (2 to 10000) filterNot isPerfectSquare map contFractLen count (_ % 2 == 1)
  }

  // based on algorithm below
  def contFractLen(n: Int) = {
    val a0 = math.sqrt(n).toInt
    val m1 = a0
    val d1 = n - m1 * m1
    val a1 = (a0 + m1) / d1

    @annotation.tailrec
    def loop(m: Int, d: Int, a: Int, cnt: Int): Int = {
      val mNext = d * a - m
      val dNext = (n - mNext * mNext) / d
      val aNext = (a0 + mNext) / dNext

      // when (m,d,a) repeats, the repeating sequence is starting again
      if (mNext != m1 || dNext != d1 || aNext != a1) loop(mNext, dNext, aNext, cnt + 1)
      else cnt
    }

    loop(m1, d1, a1, 1)
  }

  //not used
  class SqrContFractionIterator(n: Int) extends Iterator[Int] {
    //http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
    val hasNext: Boolean = true
    val a0 = math.sqrt(n).toInt

    var m = 0
    var d = 1
    var a = a0

    def next(): Int = {
      val res = a
      m = d * a - m
      d = (n - m * m) / d
      a = (a0 + m) / d
      res
    }
  }

}

