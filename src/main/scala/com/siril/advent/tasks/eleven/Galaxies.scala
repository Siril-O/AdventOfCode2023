package com.siril.advent.tasks.eleven

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

object Galaxies extends Task[Array[Array[Char]], Long] {
  override def solve(input: Array[Array[Char]]): Long =
    performCalculation(input, 2)

  override def solveAdvanced(input: Array[Array[Char]]): Long = performCalculation(input, 1000000)

  private def performCalculation(input: Array[Array[Char]], expandRatio: Int) = {
    val simplify = input.map(_.map(_ == '#'))
    //    println(simplify.map(_.map(v => if (v) "#" else '.').mkString).mkString("\n"))
    val expanded = expand(simplify, expandRatio)
    //    println(expanded.map(_.map(v => if (v) "#" else '.').mkString).mkString("\n"))
    val coords = expanded.zipWithIndex.flatMap { case (k, i) => k.zipWithIndex.collect { case (true, j) => i -> j } }
    val coordsMap = coords.indices.zip(coords).toMap
    val tuples = coords.indices.flatMap(i => coords.indices.map(j => Math.min(i, j) -> Math.max(i, j))).distinct
    tuples.map { case (a, b) => calcMinDistance(coordsMap(a), coordsMap(b)) }.sum
  }

  private def calcMinDistance(a: (Int, Int), b: (Int, Int)): Long = {
    val (ax, ay) = a
    val (bx, by) = b
    Math.abs(ax - bx) + Math.abs(ay - by)
  }

  private def expand(input: Array[Array[Boolean]], expandRatio: Int): Array[Array[Boolean]] = {
    val withRowsAdded = input.flatMap {
      v => if (v.forall(v => !v)) (0 until expandRatio).map(_ => v).toArray else Array(v)
    }
    val emptyCol = ArrayBuffer[Int]()

    for (i <- withRowsAdded.head.indices) {
      if (withRowsAdded.indices.forall(j => !withRowsAdded(j)(i))) {
        emptyCol += i
      }
    }
    val expanded = withRowsAdded.map(v => v.zipWithIndex.flatMap {
      case (v, i) => if (emptyCol.contains(i)) (0 until expandRatio).map(_ => v).toArray else Array(v)
    })
    expanded
  }


  override def parseInput(raw: List[String]): Array[Array[Char]] =
    raw.map(_.toCharArray).toArray

  override def subDir: String = "eleven"
}
