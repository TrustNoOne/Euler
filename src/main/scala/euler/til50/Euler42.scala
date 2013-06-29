package euler
package til50

import Utils._

object Euler42 extends EulerProblem {
  def wordvalue(w: String) = w.foldLeft(0)(_ + _.toInt - 64)

  override def result = withResource("words.txt") { file =>
    val words = file.mkString.replaceAll("\"", "").split(",")
    file.close
    words.map(wordvalue).filter(isTriangular).length
  }
}
