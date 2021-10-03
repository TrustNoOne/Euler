package euler
package til60

object Euler57 extends EulerProblem {

  class BigRational(val num: BigInt, val den: BigInt) {
    def +(that: BigRational) =
      new BigRational(num * that.den + that.num * den, den * that.den)
    def +(that: BigInt): BigRational = this + BigRational(that)

    override def toString = s"$num/$den"
  }

  object BigRational {
    def apply(n: BigInt, d: BigInt): BigRational = new BigRational(n, d)
    def apply(n: BigInt): BigRational = apply(n, 1)
  }

  override def result() = {
    lazy val expansionsMinusOne: LazyList[BigRational] =
      BigRational(0) #:: expansionsMinusOne.map(n => BigRational(n.den, n.num + n.den * 2))

    val expansions = (1 to 1000).map(expansionsMinusOne).map(_ + 1)
    expansions.filter(n => n.num.toString.length > n.den.toString.length).size
  }

}
