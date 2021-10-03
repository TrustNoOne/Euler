package euler

trait EulerProblem {
  def result(): Any

  def main(args: Array[String]): Unit = {
    val (elapsedTime, res) = Utils.elapsed(result())
    println(s"$getClass completed in $elapsedTime ms. Result: $res")
  }
}
