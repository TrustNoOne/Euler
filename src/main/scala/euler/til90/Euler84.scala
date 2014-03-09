package euler
package til90

import scala.util.Random

object Euler84 extends EulerProblem {
  import collection.mutable

  //   00     01   02    03    04   05   06    07    08   09
  //  'GO,   'A1, 'CC1, 'A2,  'T1, 'R1, 'B1,  'CH1, 'B2, 'B3,
  //   10     11   12    13    14   15   16    17    18   19
  //  'JAIL, 'C1, 'U1,  'C2,  'C3, 'R2, 'D1,  'CC2, 'D2, 'D3,
  //   20     21   22    23    24   25   26    27    28   29   
  //  'FP,   'E1, 'CH2, 'E2,  'E3, 'R3, 'F1,  'F2,  'U2, 'F3,
  //   30     31   32    33    34   35   36    37    38   39 
  //  'G2J,  'G1, 'G2,  'CC3, 'G3, 'R4, 'CH3, 'H1,  'T2, 'H2

  val rnd = new Random
  val landings = Array.ofDim[Int](40)

  override def result = {
    var currPos = 0
    // simulate 1 million rolls
    for (i <- 1 to 1000000) {
      currPos = evalLanding((currPos + roll()) % 40)
      landings(currPos) += 1
    }

    val mostPopular = landings.zipWithIndex.sortBy(_._1).takeRight(3).map(_._2)
    f"${mostPopular(2)}%02d${mostPopular(1)}%02d${mostPopular(0)}%02d"
  }

  def roll() = rnd.nextInt(4) + rnd.nextInt(4) + 2

  def evalLanding(currPos: Int) = currPos match {
    case 7 | 22 | 36 => evalCH(currPos)
    case 2 | 17 | 33 => evalCC(currPos)
    case 30 => 10 // G2J
    case _ => currPos
  }

  def evalCH(currPos: Int): Int = rnd.nextInt(16) match {
    case 0 => 0 // GO
    case 1 => 10 // JAIL
    case 2 => 11 // C1
    case 3 => 24 // E3
    case 4 => 39 // H2
    case 5 => 5
    case 6 | 7 if currPos == 7 => 15
    case 6 | 7 if currPos == 22 => 25
    case 6 | 7 => 5
    case 8 if currPos == 7 => 12
    case 8 if currPos == 22 => 28
    case 8 => 12
    case 9 => currPos - 3
    case _ => currPos
  }

  def evalCC(currPos: Int): Int = rnd.nextInt(16) match {
    case 0 => 0 // GO
    case 1 => 10 // JAIL
    case _ => currPos
  }

}


