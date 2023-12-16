package com.siril.advent.tasks.sixteen

import com.siril.advent.tasks.Task

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Lasers extends Task[Array[Array[Char]], Long] {

  override def solve(input: Array[Array[Char]]): Long =
    calculateEnergised(input, LaserPos(Right, 0, -1))

  override def solveAdvanced(input: Array[Array[Char]]): Long = {
    val positions: Array[LaserPos] = {
      input.head.indices.flatMap(y => Array(LaserPos(Down, -1, y), LaserPos(Up, input.length, y))) ++
        input.indices.flatMap(x => Array(LaserPos(Right, x, -1), LaserPos(Left, x, input.length)))
    }.toArray

    val calculated = positions.map(calculateEnergised(input, _))
    calculated.max
  }

  private def calculateEnergised(input: Array[Array[Char]], initial: LaserPos): Int = {
    var curLPs = Array(initial)
    val allVisited = mutable.Set.empty[(Direction, Int, Int)]
    while (curLPs.nonEmpty) {
      val newPositions = ArrayBuffer[LaserPos]()
      for (p <- curLPs) {
        val next = nextLaserPos(p, input)
        val excludeCyclic = next.collect { case v if !allVisited.contains(v.asTuple) =>
          allVisited.addOne(v.asTuple)
          v
        }
        newPositions.addAll(excludeCyclic)
      }
      curLPs = newPositions.toArray
    }
    val allVisitedCoords = allVisited.map(v => v._2 -> v._3).filterNot(v => isTerminated(v._1, v._2, input))
    allVisitedCoords.size
  }

  private def printEnergised(allVisitedCoords: Set[(Int, Int)], input: Array[Array[Char]]): Unit =
    println(
      input.zipWithIndex.map { case (v, x) => v.indices.map(y => if (allVisitedCoords(x -> y)) '#' else '.').mkString }.mkString("\n")
    )

  def isTerminated(x: Int, y: Int, map: Array[Array[Char]]): Boolean = {
    x < 0 || x >= map.length || y < 0 || y >= map.head.length
  }

  def nextLaserPos(laser: LaserPos, map: Array[Array[Char]]): Array[LaserPos] = {
    if (isTerminated(laser.nextX, laser.nextY, map)) {
      Array.empty
    } else {
      val symbol = map(laser.nextX)(laser.nextY)
      symbol match {
        case '.' => Array(laser.next())
        case '|' =>
          if (laser.direction == Left || laser.direction == Right)
            Array(laser.next(Up), laser.next(Down))
          else
            Array(laser.next())
        case '-' =>
          if (laser.direction == Up || laser.direction == Down)
            Array(laser.next(Left), laser.next(Right))
          else
            Array(laser.next())
        case '/' => Array(laser.direction match {
          case Down => laser.next(Left)
          case Left => laser.next(Down)
          case Right => laser.next(Up)
          case Up => laser.next(Right)
        })
        case '\\' => Array(laser.direction match {
          case Down => laser.next(Right)
          case Left => laser.next(Up)
          case Right => laser.next(Down)
          case Up => laser.next(Left)
        })
        case v => throw new IllegalArgumentException(s"Impossible: $v")
      }
    }

  }

  override def parseInput(raw: List[String]): Array[Array[Char]] =
    raw.map(_.toCharArray).toArray

  override def subDir: String = "sixteen"
}


case class LaserPos(direction: Direction, x: Int, y: Int) {
  val nextX = direction match {
    case Down => x + 1
    case Up => x - 1
    case _ => x
  }
  val nextY = direction match {
    case Left => y - 1
    case Right => y + 1
    case _ => y
  }

  def next(direction: Direction = direction): LaserPos = LaserPos(direction, nextX, nextY)


  def asTuple: (Direction, Int, Int) = (direction, x, y)
}

sealed trait Direction

object Left extends Direction

object Right extends Direction

object Up extends Direction

object Down extends Direction