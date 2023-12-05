package com.siril.advent.tasks.three

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

class EngineFixer extends Task[Array[Array[Char]], Int] {
  private val digits = "0123456789".toCharArray.toSet

  override def apply(matrix: Array[Array[Char]]): Int =
    findNumbers(matrix).filter(_.hasAdjacentSymbols(matrix)).map(_.value).sum

  def calculateGearRatio(matrix: Array[Array[Char]]): Int = {
    val numbers = findNumbers(matrix).groupBy(_.rowIndex)
    val asteriskCoords = matrix.zipWithIndex.collect {
      case (row, index) =>
        row.zipWithIndex.collect { case (el, rIndex) if el == '*' => (index, rIndex) }
    }.flatten
    asteriskCoords.map {
      case (row, col) => findAdjacentNumbers(row, col, matrix, numbers)
    }.filter(_.length == 2).map(_.map(_.value).product).sum
  }

  private def findAdjacentNumbers(row: Int, col: Int, matrix: Array[Array[Char]], numbers: Map[Int, Array[RowNumber]]) = {
    val adjasentRowsNumbers: Array[RowNumber] =
      (if (row == 0) Array.empty[RowNumber] else numbers.getOrElse(row - 1, Array.empty)) ++
        numbers.getOrElse(row, Array.empty) ++
        (if (row == matrix.length - 1) Array.empty[RowNumber] else numbers.getOrElse(row + 1, Array.empty))
    adjasentRowsNumbers.filter(_.isCoordsAdjasent(row, col))
  }

  private def findNumbers(matrix: Array[Array[Char]]) =
    matrix.zipWithIndex.flatMap {
      case (row, index) => findNumbersInRow(row, index)
    }

  private def getAdjasentCoordinates(rowIndex: Int, startIndex: Int, endIndex: Int, matrix: Array[Array[Char]]) =
    (
      (startIndex - 1 to endIndex + 1).map((rowIndex - 1, _)) ++
        List((rowIndex, startIndex - 1), (rowIndex, endIndex + 1)) ++
        (startIndex - 1 to endIndex + 1).map((rowIndex + 1, _))
      ).filter { case (i, j) =>
      (i >= 0 && i < matrix.length) && (j >= 0 && j < matrix(i).length)
    }
      .toList


  private def findNumbersInRow(row: Array[Char], rowIndex: Int) = {
    val numbers = new ArrayBuffer[RowNumber]()
    var currentNumber = new ArrayBuffer[Char]
    var currentNumberStart: Int = -1
    for ((el, index) <- row.zipWithIndex) {
      if (digits(el)) {
        if (currentNumberStart == -1) {
          currentNumberStart = index
        }
        currentNumber += el
        if (row.length - 1 == index || !digits(row(index + 1))) {
          numbers += RowNumber(rowIndex, currentNumberStart, index, currentNumber.mkString.toInt)
          currentNumber = new ArrayBuffer[Char]
          currentNumberStart = -1
        }
      }
    }
    numbers
  }

  case class RowNumber(rowIndex: Int, startIndex: Int, endIndex: Int, value: Int) {
    def hasAdjacentSymbols(matrix: Array[Array[Char]]): Boolean =
      getAdjasentCoordinates(rowIndex, startIndex, endIndex, matrix).exists { case (i, j) => matrix(i)(j) != '.' }

    def isCoordsAdjasent(row: Int, col: Int): Boolean =
      (row <= rowIndex + 1 && row >= rowIndex - 1) && (col >= startIndex - 1 && col <= endIndex + 1)
  }

}
