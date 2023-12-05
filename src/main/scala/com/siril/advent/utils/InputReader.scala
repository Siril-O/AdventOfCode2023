package com.siril.advent.utils

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.four.Card
import com.siril.advent.tasks.two.{Colors, Game, GameSet}

import java.util.regex.Pattern
import scala.io.Source

object InputReader {

  def readLines[T](fileName: String, task: Task[T, _]): T =
    task.parseInput(readFile(s"tasks/${task.subDir}/$fileName").toList)

  private def readFile(path: String) =
    Source.fromResource(path).getLines.filter(_.nonEmpty)

//  def readTaskTwo(fileName: String): List[Game] = {
//    val gameRegexp = "Game\\s*(\\d+)\\s*:(.+)".r
//    val gameSetRegexp = "\\s*(\\d+)\\s*(blue|red|green).*".r
//
//    readFile(s"tasks/two/$fileName").map {
//      case gameRegexp(gameId, rest) => Game(gameId.toInt, rest.split(";").map(set =>
//        GameSet(set.split(",").map {
//          case gameSetRegexp(quantity, Colors(colour)) => quantity.toInt -> colour
//          case other => throw new RuntimeException(s"wrong game set format, $other")
//        }.toList
//        )
//      ).toList)
//      case other => throw new RuntimeException(s"wrong line format, $other")
//    }.toList
//  }

//  def readTaskThree(fileName: String): Array[Array[Char]] =
//    readFile(s"tasks/three/$fileName").map(_.toCharArray).toArray

//  def readTaskFour(fileName: String): Array[Card] = {
//    val cardRegexp = "Card\\s*(\\d+)\\s*:(.+)".r
//
//    def readCard(wining: String) =
//      wining.split(" ").filter(_.nonEmpty).map(_.trim.toInt)
//
//    readFile(s"tasks/four/$fileName").map {
//      case cardRegexp(id, rest) =>
//        rest.split(Pattern.quote("|")).toList match {
//          case List(wining, actual) =>
//            Card(id, readCard(wining), readCard(actual))
//        }
//      case other => throw new IllegalArgumentException(s"Wrong input provided: $other")
//    }.toArray
//  }


  """seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    |
    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4
    |
    |water-to-light map:
    |88 18 7
    |18 25 70
    |
    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13
    |
    |temperature-to-humidity map:
    |0 69 1
    |1 0 69
    |
    |humidity-to-location map:
    |60 56 37
    |56 93 4""".stripMargin
}

