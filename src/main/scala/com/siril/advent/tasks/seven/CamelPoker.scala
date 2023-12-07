package com.siril.advent.tasks.seven

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.seven.SevenF.{Input, Output}

object Seven extends Task[Input, Output] {

  private implicit val handOrdering: Ordering[Hand] = { (x: Hand, y: Hand) =>
    val byCombo = x.combo.rank.compare(y.combo.rank)
    if (byCombo == 0) {
      x.indexedCards.zip(y.indexedCards).collectFirst { case (a, b) if a.compare(b) != 0 => a.compare(b) }.getOrElse(0)
    } else byCombo
  }

  override def solve(input: Input): Output =
    calculate(input, withJokers = false)

  override def solveAdvanced(input: Input): Output =
    calculate(input, withJokers = true)


  private def calculate(input: Input, withJokers: Boolean) = {
    val sorted = input.map { case (c, b) => (if (withJokers) Hand.applyWithJoker(c) else Hand(c)) -> b }
      .toSeq.sortBy(v => v._1)(handOrdering.reverse)

    val ranked = sorted.zip((1 to sorted.length).reverse)
    ranked.map { case ((_, bid), score) => score * bid }.sum
  }


  override def parseInput(raw: List[String]): Input = {
    raw.map(line =>
      line.split(" ").toList match {
        case List(hand, bid) => hand.toCharArray -> bid.toLong
      }
    ).toMap
  }


  override def subDir: String = "seven"

}


object SevenF {
  type Input = Map[Array[Char], Long]
  type Output = Long
}

case class Hand(cards: Array[Char], indexedCards: Array[Int], combo: Combo)

object Hand {

  private val cards = Array('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
  private val cardsMapping = cards.reverse.zipWithIndex.toMap

  val cardsWithJoker = Array('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')
  val cardsWithJokerMapping = cardsWithJoker.reverse.zipWithIndex.toMap

  def apply(cards: Array[Char]): Hand = {
    val indexedCards = cards.map(cardsMapping(_))
    Hand(cards, indexedCards, Combos.resolveCombo(indexedCards))
  }

  def applyWithJoker(cards: Array[Char]): Hand = {
    val indexedJoker = 0
    val indexedCards = cards.map(cardsWithJokerMapping(_))
    if (indexedCards.contains(indexedJoker))
      Hand(cards, indexedCards, Combos.resolveComboWithJoker(indexedCards, indexedJoker))
    else
      Hand(cards, indexedCards, Combos.resolveCombo(indexedCards))
  }
}

sealed trait Combo {
  def rank: Int
}

object Combos {
  private val reverseOrdering = implicitly[Ordering[Int]].reverse

  def resolveCombo(indexedCards: Array[Int]): Combo = {
    val indexToQuantity = indexedCards.groupBy(identity).map { case (_, arr) => arr.length }.toList.sorted(reverseOrdering)
    indexToQuantity match {
      case List(_) => FiveOfAKind
      case List(4, _) => FourOfAKind
      case List(3, 2) => FullHouse
      case List(3, _, _) => ThreeOfAKind
      case List(2, 2, _) => TwoPair
      case List(2, _, _, _) => OnePair
      case _ => HighCard
    }
  }

  def resolveComboWithJoker(indexedCards: Array[Int], indexedJoker: Int): Combo = {
    val indexToQuantity = indexedCards.groupBy(identity).map { case (i, arr) => i -> arr.length }
    val jokersQuantity = indexToQuantity(indexedJoker)
    val nonJokersQuantity = indexToQuantity.filter(_._1 != indexedJoker).values.toList
    lazy val resolvedCombo = resolveCombo(indexedCards)

    jokersQuantity match {
      case 0 => resolvedCombo
      case 1 => resolvedCombo match {
        case FullHouse | ThreeOfAKind => FourOfAKind
        case TwoPair => FullHouse
        case OnePair => ThreeOfAKind
        case HighCard => OnePair
        case _ => FiveOfAKind
      }
      case 2 => nonJokersQuantity match {
        case List(_, _, _) => ThreeOfAKind
        case List(_, _) => FourOfAKind
        case List(_) => FiveOfAKind
      }
      case 3 => nonJokersQuantity match {
        case List(_, _) => FourOfAKind
        case List(_) => FiveOfAKind
      }
      case _ => FiveOfAKind
    }
  }

  object FiveOfAKind extends Combo {
    override def rank: Int = 7
  }

  object FourOfAKind extends Combo {
    override def rank: Int = 6
  }

  object FullHouse extends Combo {
    override def rank: Int = 5
  }

  object ThreeOfAKind extends Combo {
    override def rank: Int = 4
  }

  object TwoPair extends Combo {
    override def rank: Int = 3
  }

  object OnePair extends Combo {
    override def rank: Int = 2
  }

  object HighCard extends Combo {
    override def rank: Int = 1
  }
}
