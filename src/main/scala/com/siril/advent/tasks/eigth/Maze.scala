package com.siril.advent.tasks.eigth

import com.siril.advent.tasks.Task

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MazeNavigator extends Task[Maze, Long] {
  override def solve(maze: Maze): Long = {
    var endFound = false
    var result = 0L
    var curr = maze.start

    while (!endFound) {
      maze.moves.foreach { m =>
        if (curr != maze.end) {
          curr = if (m) maze.directions(curr)._1 else maze.directions(curr)._2
          result = result + 1
        } else {
          endFound = true
        }
      }
    }
    result
  }

  override def solveAdvanced(maze: Maze): Long = {
    val seeds = maze.startNodes.map(findAllMovesWithEnd(maze, _, 2)).map(s => s(1) - s(0))
    Utils.calcLCMFast(seeds)
  }

  private def findAllMovesWithEnd(maze: Maze, startNode: Int, limit: Int): Array[Long] = {
    var currentMove = 0L
    var curr = startNode
    val result = new ArrayBuffer[Long](limit)
    while (result.length < limit) {
      maze.moves.foreach { m =>
        if (result.length < limit) {
          currentMove = currentMove + 1
          curr = if (m) maze.directions(curr)._1 else maze.directions(curr)._2
          if (maze.endNodes(curr)) result += currentMove
        }
      }
    }
    result.toArray
  }

  private val regexp = "\\s*([0-9A-Z]{3})\\s*=\\s*\\(([0-9A-Z]{3}),\\s*([0-9A-Z]{3})\\)\\s*".r

  override def parseInput(raw: List[String]): Maze = {
    raw match {
      case head :: rest =>
        val directions = rest.filter(_.nonEmpty).map {
          case regexp(start, left, right) => (start, (left, right))
          case other => throw new IllegalArgumentException(s"Wrong input direction: $other")
        }
        val allNodes = directions.map(_._1).distinct.sorted.zipWithIndex.toMap
        val indexed = directions.map { case (start, (left, right)) => (allNodes(start), (allNodes(left), allNodes(right))) }.toMap

        Maze(
          moves = head.trim.toCharArray.map(c => if (c == 'L') true else false),
          directions = indexed,
          start = allNodes.getOrElse("AAA", 0), //todo
          end = allNodes.getOrElse("ZZZ", 0),
          startNodes = allNodes.filter(_._1.last == 'A').values.toArray,
          endNodes = allNodes.filter(_._1.last == 'Z').values.toSet,
        )
      case other => throw new IllegalArgumentException(s"Wrong input $other")
    }
  }

  override def subDir: String = "eight"
}

case class Maze(
                 moves: Array[Boolean],
                 directions: Map[Int, (Int, Int)],
                 start: Int,
                 end: Int,
                 startNodes: Array[Int],
                 endNodes: Set[Int])


object Utils {

  def calcLCM(numbers: Array[Long]): Long = {
    val factors = numbers.map(calcPrimeFactors).flatMap(_.toSeq)
    val maxPowers = factors.groupBy(_._1).map { case (factor, powers) => factor -> powers.map(_._2).max }
    maxPowers.map { case (factor, power) => Math.pow(factor, power) }.product.toLong
  }

  def calcLCMFast(numbers: Array[Long]): Long = 
    numbers.reduce((a, b) => (a * b) / gcd(a, b))


  def calcPrimeFactors(number: Long): Map[Long, Long] = {
    val factors = mutable.Map.empty[Long, Long]
    var rest = number
    for (factor <- 2L to rest) {
      while (rest % factor == 0) {
        factors.put(factor, factors.getOrElse(factor, 0L) + 1)
        rest = rest / factor
      }
    }
    factors.toMap
  }

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (a == 0 || b == 0) a + b
    else {
      val biggerValue = Math.max(a, b)
      val smallerValue = Math.min(a, b)
      gcd(biggerValue % smallerValue, smallerValue);
    }

}

