package com.siril.advent.tasks.six

import com.siril.advent.tasks.Task

object Race extends Task[Map[Long, Long], Long] {

  override def solve(games: Map[Long, Long]): Long =
    calculate(games)

  private def calculate(games: Map[Long, Long]) = {
    val winnerGames = games.map { case (time, distance) =>
      (
        calculateGames((1L until time / 2).reverseIterator, time, distance) ++
        calculateGames((time / 2 until time).iterator, time, distance)
        ).length
    }
    winnerGames.product
  }

  override def solveAdvanced(games: Map[Long, Long]): Long =
    calculate(Map(games.keys.mkString.toLong -> games.values.mkString.toLong))

  private def calculateGames(holds: Iterator[Long], time: Long, distance: Long): Array[Long] =
    holds.map(calculateGame(_, time)).takeWhile(_ > distance).toArray

  private def calculateGame(holdTime: Long, time: Long): Long =
    holdTime * (time - holdTime)

  override def parseInput(raw: List[String]): Map[Long, Long] =
    raw.map(_.split(":").last) match {
      case List(time, distance) =>
        val times = time.split(" ").filter(_.nonEmpty).map(_.toLong)
        val distances = distance.split(" ").filter(_.nonEmpty).map(_.toLong)
        times.zip(distances).toMap
      case _ => throw new IllegalArgumentException("Wrong inout")
    }

  override def subDir: String = "six"

}
