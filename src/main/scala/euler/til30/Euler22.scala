package euler
package til30

import Utils._

object Euler22 extends EulerProblem {
  def alphabeticalValue(s: String) = s.foldLeft(0)(_ + _.toInt - 64)
  override def result = withResource("names.txt") { file =>
    val list = file.mkString.replaceAll("\"", "").split(",")
    file.close
    list.sorted.zipWithIndex.foldLeft(0) {
      (acc, nameIdx) => acc + alphabeticalValue(nameIdx._1) * (nameIdx._2 + 1)
    }
  }
}
