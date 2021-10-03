package euler
package til100

import euler.Utils.withResource

object Euler99 extends EulerProblem {

  val nums = withResource("p099_base_exp.txt") { f =>
    f.getLines()
      .zipWithIndex
      .map { case (l, i) =>
        val ints = l.split(",").map(_.toInt)
        (i + 1, ints(0), ints(1))
      }
      .toList
  }

  override def result() = {
    nums
      .sortWith { case ((_, b1, e1), (_, b2, e2)) =>
        math.pow(b2.toDouble, e2.toDouble / e1) < b1
      }
      .head
      ._1
  }

}
