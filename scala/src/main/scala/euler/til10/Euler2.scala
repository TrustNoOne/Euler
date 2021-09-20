package euler
package til10

object Euler2 extends EulerProblem {
  override def result = (fib filter (_ % 2 == 0) takeWhile (_ < 4000000)).sum

  val fib: LazyList[Int] = 1 #:: 2 #:: (fib zip (fib.tail) map {
    case (x, y) => x + y
  })
}
