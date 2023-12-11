package com.siril.advent.tasks.eleven

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

object Galaxies extends Task[Array[Array[Char]], Long] {
  override def solve(input: Array[Array[Char]]): Long =
    performCalculation(input, 2)

  override def solveAdvanced(input: Array[Array[Char]]): Long = performCalculation(input, 1000000)

  private def performCalculation(input: Array[Array[Char]], expandRatio: Int) = {
    val initialCoords = input.zipWithIndex.flatMap { case (k, i) => k.zipWithIndex.collect { case (v, j) if v == '#' => i.toLong -> j.toLong } }
    val expanded = expandCoords(input, initialCoords, expandRatio)

    val coordsMap = expanded.indices.zip(expanded).toMap
    val tuples = expanded.indices.flatMap(i => expanded.indices.map(j => Math.min(i, j) -> Math.max(i, j))).distinct
    tuples.map { case (a, b) => calcMinDistance(coordsMap(a), coordsMap(b)) }.sum
  }

  private def calcMinDistance(a: (Long, Long), b: (Long, Long)): Long = {
    val (ax, ay) = a
    val (bx, by) = b
    Math.abs(ax - bx) + Math.abs(ay - by)
  }


  private def expandRows(input: Array[Array[Char]], coords: Array[(Long, Long)], expandRatio: Long): Array[(Long, Long)] = {
    val emptyRows = input.zipWithIndex.collect {
      case (v, i) if v.forall(_ == '.') => i
    }
    val expanded = new ArrayBuffer[(Long, Long)]()
    var toExpand = coords
    for ((i, index) <- emptyRows.zipWithIndex) {
      val rest = new ArrayBuffer[(Long, Long)]()
      for ((x, y) <- toExpand)
        if (x > i + (expandRatio * index))
          rest += (x + expandRatio) -> y
        else
          expanded += (x -> y)
      toExpand = rest.toArray
    }
    expanded.toArray ++ toExpand
  }

  private def expandColumns(input: Array[Array[Char]], coords: Array[(Long, Long)], expandRatio: Long): Array[(Long, Long)] = {
    val emptyCols = ArrayBuffer[Int]()
    for (i <- input.head.indices)
      if (input.indices.forall(j => input(j)(i) == '.')) emptyCols += i

    val expanded = new ArrayBuffer[(Long, Long)]()
    var toAddCols = coords
    for ((i, index) <- emptyCols.zipWithIndex) {
      val rest = new ArrayBuffer[(Long, Long)]()
      for ((x, y) <- toAddCols)
        if (y > i + (expandRatio * index))
          rest += (x -> (y + expandRatio))
        else
          expanded += (x -> y)
      toAddCols = rest.toArray
    }
    expanded.toArray ++ toAddCols
  }

  private def expandCoords(input: Array[Array[Char]], coords: Array[(Long, Long)], expandRatio: Long): Array[(Long, Long)] = {
    val withRowsAdded = expandRows(input, coords, expandRatio - 1)
    expandColumns(input, withRowsAdded, expandRatio - 1)
  }

  override def parseInput(raw: List[String]): Array[Array[Char]] =
    raw.map(_.toCharArray).toArray

  override def subDir: String = "eleven"
}
