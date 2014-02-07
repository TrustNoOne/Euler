package euler
package til70

import euler._

object Euler62 extends EulerProblem {

  override def result = {
    // for n > 2097151L n^3 overflows a long int
    val cubes = (1L to 2097151L).map(x => x * x * x)
    // group cubes by their digits
    val res = cubes.groupBy(_.toString.sorted).filter(_._2.size == 5).flatMap(_._2)

    res.min
  }

}

