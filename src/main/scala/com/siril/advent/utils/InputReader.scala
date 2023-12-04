package com.siril.advent.utils

import com.siril.advent.tasks.two.{Colors, Game, GameSet}

import scala.io.Source

object InputReader {
  def readLines(fileName: String): List[String] =
    Source.fromResource(s"tasks/one/$fileName").getLines.filter(_.nonEmpty).toList


  private val gameRegexp = "Game\\s*(\\d+)\\s*:(.+)".r
  private val gameSetRegexp = "\\s*(\\d+)\\s*(blue|red|green).*".r

  def readTaskTwo(fileName: String): List[Game] =
    Source.fromResource(s"tasks/two/$fileName").getLines.filter(_.nonEmpty).map {
      case gameRegexp(gameId, rest) => Game(gameId.toInt, rest.split(";").map(set =>
        GameSet(set.split(",").map {
          case gameSetRegexp(quantity, Colors(colour)) => quantity.toInt -> colour
          case other => throw new RuntimeException(s"wrong game set format, $other")
        }.toList
        )
      ).toList)
      case other => throw new RuntimeException(s"wrong line format, $other")
    }.toList
}

