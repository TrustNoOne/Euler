package euler
package til10

object Euler6 extends EulerProblem {
  override def result = {
    val n = 100
    val sum = (n * (n + 1)) / 2
    val squareOfSum = sum * sum
    val sumOfSquare = (1 to n).reduceLeft((a, b) => a + b * b)
    squareOfSum - sumOfSquare
  }
}