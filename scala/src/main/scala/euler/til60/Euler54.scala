package euler
package til60

import Utils._
import scala.collection.immutable.SortedMap

object Euler54 extends EulerProblem {
  sealed trait Suit
  object Suit {
    def apply(s: Char) = s match {
      case 'H' => Hearts
      case 'D' => Diamonds
      case 'C' => Clubs
      case 'S' => Spades
    }
  }
  case object Hearts extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Spades extends Suit

  case class Card(value: Int, suit: Suit) {
    override def toString = f"$value%2d${suit.toString.head}"
  }
  case class Hand(p1cards: collection.Seq[Card]) {
    lazy val vals = p1cards.groupBy(_.value).view.mapValues(_.map(_.suit))
    lazy val suits = p1cards.groupBy(_.suit).view.mapValues(_.map(_.value))
    lazy val cardCounts = p1cards.foldLeft(SortedMap.empty[Int, Int]) {
      (counts, card) =>
        counts + (card.value -> (counts.getOrElse(card.value, 0) + 1))
    }

    override def toString = {
      "[" + p1cards.sortBy(_.value).mkString(",") + "]"
    }
  }

  private def charToVal(c: Char): Int = c match {
    case '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => c.toInt - 48
    case 'T'                                           => 10
    case 'J'                                           => 11
    case 'Q'                                           => 12
    case 'K'                                           => 13
    case 'A'                                           => 14
  }

  private def parseHands() = withResource("poker.txt") { src =>
    src
      .getLines()
      .map { line =>
        val cards = line.split(" ") map { card =>
          Card(charToVal(card(0)), Suit(card(1)))
        }
        val hands = cards.grouped(5) // map { _.sortBy(_.value) }
        (Hand(hands.next()), Hand(hands.next()))
      }
      .toVector
  }

  private def pow(n: Int, m: Int) = math.pow(n.toDouble, m.toDouble).toInt

  /**
    * High Card: Highest value card.
    * One Pair: Two cards of the same value.
    * Two Pairs: Two different pairs.
    * Three of a Kind: Three cards of the same value.
    * Straight: All cards are consecutive values.
    * Flush: All cards of the same suit.
    * Full House: Three of a kind and a pair.
    * Four of a Kind: Four cards of the same value.
    * Straight Flush: All cards are consecutive values of same suit.
    * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
    */
  private def eval(h: Hand) = {
    //very stupid ranking alg
    val cardCounts = h.cardCounts
    val inverseCardCounts =
      cardCounts.groupBy(_._2).view.mapValues(_.keys.toList.sorted)
    val highestCard = cardCounts.last._1
    val lowestCard = cardCounts.head._1

    val isFlush = h.suits.size == 1

    val rankScore = cardCounts.values.toSeq.sorted match {
      case Seq(1, 4) => // four of a kind
        7000000 + inverseCardCounts(1).sum + 14 * inverseCardCounts(4).sum
      case Seq(2, 3) => // full 		house
        6000000 + inverseCardCounts(2).sum + 14 * inverseCardCounts(3).sum
      case Seq(1, 1, 3) => // three of a kind
        3000000 + pow(14, 2) * inverseCardCounts(3).sum +
          inverseCardCounts(1).zipWithIndex.map(x => pow(14, x._2) * x._1).sum
      case Seq(1, 2, 2) => // two pair
        2000000 + inverseCardCounts(1).sum +
          inverseCardCounts(1).zipWithIndex
            .map(x => pow(14, x._2 + 1) * x._1)
            .sum
      case Seq(1, 1, 1, 2) => // pair
        1000000 + pow(14, 3) * inverseCardCounts(2).sum +
          inverseCardCounts(1).zipWithIndex.map(x => pow(14, x._2) * x._1).sum
      case _ => //high card or straight/flush/royal
        inverseCardCounts(1).zipWithIndex.map(x => pow(14, x._2) * x._1).sum
    }

    val isStraight = rankScore < 1000000 && ((highestCard - lowestCard == 4) || (highestCard == 14 && cardCounts
      .dropRight(1)
      .last
      ._1 == 5))

    val straightScore = if (isStraight) 4000000 else 0
    val flushScore = if (isFlush) 5000000 else 0
    val royalScore =
      if (isStraight && isFlush && highestCard == 14) 9000000 else 0

    rankScore + flushScore + straightScore + royalScore
  }

  override def result = {
    parseHands().count { case (p1Hand, p2Hand) => eval(p1Hand) > eval(p2Hand) }
  }
}
