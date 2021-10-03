package euler
package til100

import euler.Utils.withResource

object Euler98 extends EulerProblem {

  val words = withResource("p098_words.txt") { f => f.mkString.split("""[",]+""").toList.tail }

  def isSquare(word: String, mapping: Map[Char, Int]) = {
    val digits = word.map(mapping)
    digits.head != 0 && isPerfectSquare(fromDigits(digits.reverse))
  }

  // all mappings that produce a square for the given string
  def letterMappings(w: String) = {
    val letters = w.toSet
    val mappings = (0 to 9).combinations(letters.size) flatMap (_.permutations)
    mappings map (letters.zip(_).toMap) filter (isSquare(w, _))
  }

  override def result() = {
    val anagrams = words
      .groupBy(_.sorted)
      .values filter (_.size > 1) flatMap (_.combinations(2))

    anagrams.flatMap {
      case w1 :: w2 :: _ =>
        letterMappings(w1) filter (isSquare(w2, _)) flatMap { m =>
          Seq(fromDigits(w1.map(m).reverse), fromDigits(w2.map(m).reverse))
        }

      case _ => throw new IllegalStateException()
    }.max
  }

}
