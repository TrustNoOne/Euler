package euler.contfract

/**
 * Iterator for continued fraction expansion of a square root
 *
 * @see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
 */
class SqrContinuedFractionIterator(n: Int) extends Iterator[Int] {
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