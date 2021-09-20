package euler
package til90

import euler.Utils.withResource

object Euler89 extends EulerProblem {
  val LetterValues = Map('I' -> 1,
                         'V' -> 5,
                         'X' -> 10,
                         'L' -> 50,
                         'C' -> 100,
                         'D' -> 500,
                         'M' -> 1000)

  def romanToInt(romanNum: String) = {
    val (result, _) = romanNum.foldRight((0, 1)) {
      case (romanDigit, (acc, currMaxDigit)) =>
        LetterValues(romanDigit) match {
          case x if x < currMaxDigit => (acc - x, currMaxDigit)
          case x                     => (acc + x, x)
        }
    }

    result
  }

  def biggestSubValue(x: Int): (Int, String) = {
    if (x >= 1000) (1000, "M")
    else if (x >= 900) (900, "CM")
    else if (x >= 500) (500, "D")
    else if (x >= 400) (400, "CD")
    else if (x >= 100) (100, "C")
    else if (x >= 90) (90, "XC")
    else if (x >= 50) (50, "L")
    else if (x >= 40) (40, "XL")
    else if (x >= 10) (10, "X")
    else if (x >= 9) (9, "IX")
    else if (x >= 5) (5, "V")
    else if (x >= 4) (4, "IV")
    else (1, "I")
  }

  def intToRoman(x: Int, res: String = ""): String = {
    if (x > 0) {
      val (v, ls) = biggestSubValue(x)
      intToRoman(x - v, res + ls)
    } else res
  }

  override def result = withResource("p089_roman.txt") { f =>
    f.getLines()
      .map { romanNum =>
        val x = romanToInt(romanNum)
        val minimalRomanNum = intToRoman(x)
        romanNum.length - minimalRomanNum.length
      }
      .sum
  }

}
