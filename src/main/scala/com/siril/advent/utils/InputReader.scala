package com.siril.advent.utils

import com.siril.advent.tasks.two.{Colors, Game, GameSet}

import scala.io.Source

object InputReader {
  private val gameRegexp = "Game\\s*(\\d+)\\s*:(.+)".r
  private val gameSetRegexp = "\\s*(\\d+)\\s*(blue|red|green).*".r


  def readLines(fileName: String): List[String] =
    readFile(s"tasks/$fileName").toList

  private def readFile(path: String) =
    Source.fromResource(path).getLines.filter(_.nonEmpty)

  def readTaskTwo(fileName: String): List[Game] =
    readFile(s"tasks/two/$fileName").map {
      case gameRegexp(gameId, rest) => Game(gameId.toInt, rest.split(";").map(set =>
        GameSet(set.split(",").map {
          case gameSetRegexp(quantity, Colors(colour)) => quantity.toInt -> colour
          case other => throw new RuntimeException(s"wrong game set format, $other")
        }.toList
        )
      ).toList)
      case other => throw new RuntimeException(s"wrong line format, $other")
    }.toList

  def readTaskThree(fileName: String): Array[Array[Char]] =
    readFile(s"tasks/three/$fileName").map(_.toCharArray).toArray

}

