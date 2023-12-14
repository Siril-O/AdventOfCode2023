package com.siril.advent.tasks.thirteen

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.eigth.Utils.{flipMatrix, insertInArray}

import scala.collection.mutable.ArrayBuffer

object Mirrors extends Task[Array[Array[Array[Char]]], Long] {
  override def solve(input: Array[Array[Array[Char]]]): Long = {
    val indexes = input.map(solveForSingleInput)
    indexes.sum
  }

  private def findReflection(strings: Array[String], excludeIndex: Int): Option[Int] = {
    var result: Option[Int] = None
    for (i <- 1 until strings.length if result.isEmpty && excludeIndex != i) {
      var resumeSearch = true
      for (j <- 0 until Math.min(strings.length - i, i) if result.isEmpty && resumeSearch)
        if (strings(i - j - 1) != strings(i + j)) resumeSearch = false

      if (resumeSearch) result = Some(i)
    }
    result
  }

  private def findReflectionHorizontal(input: Array[Array[Char]], excludeIndex: Int = -1): Option[Int] =
    findReflection(input.map(_.mkString), excludeIndex)

  private def findReflectionVertical(input: Array[Array[Char]], excludeIndex: Int = -1): Option[Int] =
    findReflection(flipMatrix(input).map(_.mkString), excludeIndex)

  private def solveForSingleInput(input: Array[Array[Char]]): Int =
    findReflectionHorizontal(input).map(h => h * 100).orElse(findReflectionVertical(input)).get

  override def solveAdvanced(mirrors: Array[Array[Array[Char]]]): Long = {
    val old = mirrors.map(v => v -> findReflectionHorizontal(v).map(r => r -> true).orElse(findReflectionVertical(v).map(r => r -> false)))
    val found = old.map { case (mirror, Some(oldRefl)) => findSmudgedMirror(mirror, oldRefl._1, oldRefl._2) }
    found.sum
  }

  // todo just for debug
  private def buildVariants(mirror: Array[Array[Char]]): Array[Array[Array[Char]]] = {
    val variants = new ArrayBuffer[Array[Array[Char]]]()
    for (i <- mirror.indices)
      for (j <- mirror(i).indices) {
        val nweLine = insertInArray(mirror(i), j, if (mirror(i)(j) == '.') '#' else '.')
        variants.addOne(insertInArray(mirror, i, nweLine))
      }
    variants.toArray
  }

  private def findSmudgedMirror(mirror: Array[Array[Char]], excludeIndex: Int, excludeHorizontal: Boolean): Long = {
    var result = 0
    var found = false
    for (i <- mirror.indices if !found) {
      for (j <- mirror(i).indices if !found) {
        val nweLine = insertInArray(mirror(i), j, if (mirror(i)(j) == '.') '#' else '.')
        val variant = insertInArray(mirror, i, nweLine)
        val res = findReflectionHorizontal(variant, if (excludeHorizontal) excludeIndex else -1)
          .map(h => h * 100)
          .orElse(findReflectionVertical(variant, if (!excludeHorizontal) excludeIndex else -1))
        if (res.isDefined) {
          found = true
          result = res.get
        }
      }
    }
    result
  }

  override def parseInput(raw: List[String]): Array[Array[Array[Char]]] =
    raw.collect {
      case v if v.isBlank => '0'
      case v => v
    }.mkString("\n").split("0").map(_.split("\n").filterNot(_.isBlank).map(_.toCharArray))

  override def subDir: String = "thirteen"

}
