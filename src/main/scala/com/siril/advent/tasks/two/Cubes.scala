package com.siril.advent.tasks.two

import com.siril.advent.tasks.Task

class Cubes(cubesUpperBoundary: Map[Color, Int]) extends Task[List[Game], Int] {
  override def apply(games: List[Game]): Int = {
    val validGames = games.filter(
      game => game.sets.forall(
        s => s.cubes.forall {
          case (quantity, color) => cubesUpperBoundary(color) >= quantity
        }
      )
    )
    validGames.map(_.id).sum
  }

  def maxByColors(games: List[Game]): Int = {
    val validGames = games.map { game =>
      game.sets.flatMap(s => s.cubes).groupBy(_._2).map(v => v._1 -> v._2.map(_._1).max)
    }
    validGames.map(_.values.product).sum
  }
}
