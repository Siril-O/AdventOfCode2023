package com.siril.advent.tasks.two

import com.siril.advent.tasks.Task

class Cubes(cubesUpperBoundary: Map[Color, Int]) extends Task[List[Game], Int] {
  override def solve(games: List[Game]): Int = {
    val validGames = games.filter(
      game => game.sets.forall(
        s => s.cubes.forall {
          case (quantity, color) => cubesUpperBoundary(color) >= quantity
        }
      )
    )
    validGames.map(_.id).sum
  }

  override def solveAdvanced(games: List[Game]): Int = {
    val validGames = games.map { game =>
      game.sets.flatMap(s => s.cubes).groupBy(_._2).map(v => v._1 -> v._2.map(_._1).max)
    }
    validGames.map(_.values.product).sum
  }

  override def parseInput(raw: List[String]): List[Game] =
    raw.map {
      case gameRegexp(gameId, rest) => Game(gameId.toInt, rest.split(";").map(set =>
        GameSet(set.split(",").map {
          case gameSetRegexp(quantity, Colors(colour)) => quantity.toInt -> colour
          case other => throw new RuntimeException(s"wrong game set format, $other")
        }.toList
        )
      ).toList)
      case other => throw new RuntimeException(s"wrong line format, $other")
    }

  override def subDir: String = "two"

  private val gameRegexp = "Game\\s*(\\d+)\\s*:(.+)".r
  private val gameSetRegexp = "\\s*(\\d+)\\s*(blue|red|green).*".r

}
