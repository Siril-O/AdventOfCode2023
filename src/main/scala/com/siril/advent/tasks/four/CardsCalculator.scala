package com.siril.advent.tasks.four

import com.siril.advent.tasks.Task

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

object CardsCalculator extends Task[Array[Card], Int] {
  override def apply(cards: Array[Card]): Int =
    cards.filter(_.intersect.nonEmpty).map(card =>
      Math.pow(2, card.intersect.size - 1).toInt
    ).sum

  def calcIncludingCopies(input: Array[Card]): Int = {
    val cards = new ArrayBuffer() ++ input.map(c => c -> 1)
    for (index <- cards.indices) {
      val (card, quantity) = cards(index)
      if (card.intersect.nonEmpty) {
        (1 to card.intersect.size).foreach { i =>
          val (c, q) = cards(i + index)
          cards.update(i + index, (c, q + quantity))
        }
      }
    }
    cards.map { case (_, q) => q }.sum
  }
  override def parseInput(raw: List[String]): Array[Card] =
    raw.map {
      case cardRegexp(id, rest) =>
        rest.split(Pattern.quote("|")).toList match {
          case List(wining, actual) =>
            Card(id, readCard(wining), readCard(actual))
        }
      case other => throw new IllegalArgumentException(s"Wrong input provided: $other")
    }.toArray

  private def readCard(wining: String) =
    wining.split(" ").filter(_.nonEmpty).map(_.trim.toInt)

  override def subDir: String = "four"

  private val cardRegexp = "Card\\s*(\\d+)\\s*:(.+)".r
}
