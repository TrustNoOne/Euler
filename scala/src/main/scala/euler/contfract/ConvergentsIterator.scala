package euler.contfract

/**
  * Iterator for convergents of a generalized continued fraction:
  *
  * @see http://en.wikipedia.org/wiki/Convergent_(continued_fraction)
  */
class ConvergentsIterator(asIt: Iterator[Int], bsIt: Iterator[Int])
    extends Iterator[(BigInt, BigInt)] {
  def this(as: Seq[Int], bs: Seq[Int]) = this(as.iterator, bs.iterator)

  var _hasNext = asIt.hasNext && bsIt.hasNext
  def hasNext: Boolean = _hasNext

  var _Acurr = BigInt(bsIt.next())
  var _Bcurr = BigInt(1)
  var (_Anext, _Bnext) = {
    val b1 = BigInt(bsIt.next())
    (b1 * _Acurr + asIt.next(), b1)
  }

  def next(): (BigInt, BigInt) = {
    val (a_n, b_n) = (asIt.next(), bsIt.next())
    //A_n=b_n*A_n-1 + a_n*A_n-2
    val A_n = b_n * _Anext + a_n * _Acurr
    //B_n=b_n*B_n-1 + a_n*B_n-2
    val B_n = b_n * _Bnext + a_n * _Bcurr

    val result = (_Acurr, _Bcurr)
    _Acurr = _Anext
    _Bcurr = _Bnext
    _Anext = A_n
    _Bnext = B_n

    _hasNext = bsIt.hasNext && asIt.hasNext
    result
  }
}
