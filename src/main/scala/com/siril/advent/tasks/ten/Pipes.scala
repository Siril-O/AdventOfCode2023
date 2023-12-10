package com.siril.advent.tasks.ten

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.ten.Pipe.pipesSymbols

import scala.collection.mutable.ArrayBuffer

object Pipes extends Task[Array[Array[Char]], Long] {
  override def solve(input: Array[Array[Char]]): Long =
    findClosedPath(input).length / 2

  private def findClosedPath(input: Array[Array[Char]]): ArrayBuffer[Pipe] = {
    val startCoords = findCoord(input, 'S')
    val startPipeSymbol = findStartChar(input)
    val startPipe = Pipe(startCoords, startPipeSymbol, Pipe.pipeJoints(startCoords, startPipeSymbol)._1, Pipe.pipeJoints(startCoords, startPipeSymbol)._2)
    var currentPipe = startPipe

    var endFound = false
    val path = ArrayBuffer[Pipe](startPipe)
    while (!endFound) {
      Pipe.fromPrevPipe(currentPipe, input) match {
        case Some(pipe) =>
          if (startPipe.coords == pipe.coords)
            endFound = true
          path += pipe
          currentPipe = pipe
        case _ => endFound = true
      }
    }
    path
  }


  private def findStartChar(input: Array[Array[Char]]): Char = {
    val startCoords = findCoord(input, 'S')

    def has2Connections(symbol: Char): Boolean = {
      val (l, r) = Pipe.pipeJoints(startCoords, symbol)
      Pipe.fromPrevPipe(Pipe(startCoords, symbol, l, r), input).isDefined && Pipe.fromPrevPipe(Pipe(startCoords, symbol, r, l), input).isDefined
    }

    pipesSymbols.find(has2Connections).get
  }

  private def findCoord(input: Array[Array[Char]], toFind: Char) = {
    var result: Option[(Int, Int)] = None
    for (i <- input.indices if result.isEmpty) {
      for (j <- input(i).indices if result.isEmpty) {
        if (input(i)(j) == toFind) result = Some((i, j))
      }
    }
    result.get
  }

  override def solveAdvanced(input: Array[Array[Char]]): Long = {
    val paths = findClosedPath(input).toArray.map(v => v.coords -> v).toMap
    val pathWithDirections = input.indices.map(i => input(i).indices
      .map(j => if (paths.contains(i -> j)) paths(i -> j).isUp match {
        case Some(true) => '↑'
        case Some(false) => '↓'
        case None => paths(i -> j).symbol
      }
      else '.').toArray
    ).toArray

    println(pathWithDirections.map(_.mkString).mkString("\n"))

    def calcEnclosed(row: Array[Char]) = {
      val regexp = row.collectFirst {
        case '↑' => regexpB
        case '↓' => regexpA
      }.getOrElse(regexpA)

      regexp.findAllIn(row.mkString).map(v => v.length - 2).sum
    }

    val enclosed = pathWithDirections.zipWithIndex.map { case (row, index) => index -> calcEnclosed(row) }
    enclosed.map(_._2).sum
  }

  private val regexpA = "↓\\.+↑".r
  private val regexpB = "↑\\.+↓".r

  override def parseInput(raw: List[String]): Array[Array[Char]] =
    raw.filter(_.nonEmpty).map(_.toCharArray).toArray

  override def subDir: String = "ten"
}

case class Pipe(coords: (Int, Int), symbol: Char, prevJoint: (Int, Int), nextJoint: (Int, Int)) {
  val isUp: Option[Boolean] = {
    val prevX = prevJoint._1
    val nextX = nextJoint._1
    symbol match {
      case '|' | 'L' | 'J' | '7' | 'F' => Some(nextX < prevX)
      case _ => None
    }
  }
}

object Pipe {
  val pipesSymbols: Set[Char] = Set('|', '-', 'L', 'J', '7', 'F')

  def pipeJoints(coords: (Int, Int), symbol: Char): ((Int, Int), (Int, Int)) = {
    val (x, y) = coords

    symbol match {
      case '|' => (x - 1, y) -> (x + 1, y)
      case '-' => (x, y - 1) -> (x, y + 1)
      case 'L' => (x - 1, y) -> (x, y + 1)
      case 'J' => (x - 1, y) -> (x, y - 1)
      case '7' => (x, y - 1) -> (x + 1, y)
      case 'F' => (x + 1, y) -> (x, y + 1)
    }
  }

  def fromPrevPipe(pipe: Pipe, input: Array[Array[Char]]): Option[Pipe] = {
    if (pipe.nextJoint._1 < 0 || pipe.nextJoint._1 >= input.length || pipe.nextJoint._2 < 0 || pipe.nextJoint._2 >= input.head.length) {
      None
    } else {
      val symbol = input(pipe.nextJoint._1)(pipe.nextJoint._2)
      if (pipesSymbols(symbol)) {
        val (left, right) = pipeJoints(pipe.nextJoint, symbol)
        (pipe.coords, left, right) match {
          case (p, l, r) if p == l => Some(Pipe(pipe.nextJoint, symbol, l, r))
          case (p, l, r) if p == r => Some(Pipe(pipe.nextJoint, symbol, r, l))
          case _ => None
        }
      } else None
    }
  }
}

