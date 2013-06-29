package euler

import scala.reflect.internal.MissingRequirementError
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ universe => ru }

object ProjectEuler extends App {
  implicit val m = ru.runtimeMirror(getClass.getClassLoader)

  val eulerProblems = Iterator.from(1).map(eulerProblem).takeWhile(_.isDefined).flatten

  val (totalElapsed, _) = elapsed {
    eulerProblems.zipWithIndex foreach {
      case (prob, n) =>
        val (elapsedTime, result) = elapsed(prob.result)
        println(f"Problem $n%3d completed in $elapsedTime%5d ms. Result: $result")
    }
  }
  println(s"Total elapsed time: $totalElapsed ms")

  private def eulerProblem(n: Int)(implicit m: Mirror): Option[EulerProblem] = {
    // pkg for 1 to 10 = til10, 11 to 20 = til20 etc
    val pkg = 10 + (n - 1) / 10 % 10 * 10
    try {
      val module = m.staticModule(s"euler.til$pkg.Euler" + n)
      val eulerProblem = m.reflectModule(module).instance.asInstanceOf[EulerProblem]
      Some(eulerProblem)
    } catch {
      case e: MissingRequirementError =>
        None
    }
  }

  private def elapsed(block: => Any) = {
    val t0 = System.currentTimeMillis
    val result = block
    (System.currentTimeMillis - t0, result)
  }

}