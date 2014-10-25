package euler
package til60

import Utils._

object Euler59 extends EulerProblem {
  
  val keys = for {
    x <- 'a' to 'z'
    y <- 'a' to 'z'
    z <- 'a' to 'z'
  } yield Seq(x, y, z)

  def decrypt(text: Seq[Char], key: Seq[Char]) = {
    text.grouped(key.size).flatMap(_.zip(key).map(x => (x._1 ^ x._2).toChar))
  }

  override def result = withResource("cipher1.txt") { src =>
    val chars = src.mkString.split(",").map(_.trim.toInt.toChar)

    val decrypted = keys.view.map(k => decrypt(chars, k).toSeq).find { text =>
      text.containsSlice("this") && text.containsSlice("that")
    }.head

    // clear text:  decrypted.mkString
    decrypted.foldLeft(0)(_ + _)
  }

}