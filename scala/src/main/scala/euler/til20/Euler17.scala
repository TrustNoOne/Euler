package euler
package til20

object Euler17 extends EulerProblem {
  override def result() = {
    //one two three four five six seven eight nine
    val OneToNine = 3 + 3 + 5 + 4 + 4 + 3 + 5 + 5 + 4
    //eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen
    val ElevenToNineteen = 6 + 6 + 8 + 8 + 7 + 7 + 9 + 8 + 8
    val Twenty, Thirty, Eighty, Ninety = 6
    val Forty, Fifty, Sixty = 5
    val Seventy = 7
    val OneHundredAnd, TwoHundredAnd, SixHundredAnd = 13
    val ThreeHundredAnd, SevenHundredAnd, EightHundredAnd = 15
    val FourHundredAnd, FiveHundredAnd, NineHundredAnd = 14
    val OneThousand = 11

    val OneTo99 = OneToNine + 3 /*Ten*/ +
      ElevenToNineteen +
      Twenty * 10 + OneToNine +
      Thirty * 10 + OneToNine +
      Forty * 10 + OneToNine +
      Fifty * 10 + OneToNine +
      Sixty * 10 + OneToNine +
      Seventy * 10 + OneToNine +
      Eighty * 10 + OneToNine +
      Ninety * 10 + OneToNine

    val OneTo999 = OneTo99 +
      10 /*OneHundred*/ + OneHundredAnd * 99 + OneTo99 +
      10 /*TwoHundred*/ + TwoHundredAnd * 99 + OneTo99 +
      12 /*ThreeHundred*/ + ThreeHundredAnd * 99 + OneTo99 +
      11 /*FourHundred*/ + FourHundredAnd * 99 + OneTo99 +
      11 /*FiveHundred*/ + FiveHundredAnd * 99 + OneTo99 +
      10 /*SixHundred*/ + SixHundredAnd * 99 + OneTo99 +
      12 /*SevenHundred*/ + SevenHundredAnd * 99 + OneTo99 +
      12 /*EigthHundred*/ + EightHundredAnd * 99 + OneTo99 +
      11 /*NineHundred*/ + NineHundredAnd * 99 + OneTo99

    OneTo999 + OneThousand
  }
}
