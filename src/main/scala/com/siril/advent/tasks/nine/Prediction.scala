package com.siril.advent.tasks.nine

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

object Prediction extends Task[Array[Array[Int]], Long] {
  override def solve(input: Array[Array[Int]]): Long =
    input.map(predictNext).sum

  override def solveAdvanced(input: Array[Array[Int]]): Long =
    input.map(predictPrev).sum

  private def buildDiffSequences(sequence: Array[Int]): ArrayBuffer[ArrayBuffer[Int]] = {
    val subSequences: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer(ArrayBuffer(sequence: _*))
    var subSequenceIndex = 0

    while (subSequences.last.exists(_ != 0)) {
      val current = subSequences(subSequenceIndex)
      subSequences += new ArrayBuffer[Int]()
      for (index <- current.indices.drop(1)) {
        subSequences.last += current(index) - current(index - 1)
      }
      subSequenceIndex = subSequenceIndex + 1
    }
    subSequences
  }

  private def predictNext(sequence: Array[Int]): Long = {
    val subSequences = buildDiffSequences(sequence)
    for (index <- subSequences.indices.drop(1).reverse) {
      subSequences(index - 1) += subSequences(index).last + subSequences(index - 1).last
    }
    subSequences.head.last
  }

  private def predictPrev(sequence: Array[Int]): Long = {
    val subSequences = buildDiffSequences(sequence)
    for (index <- subSequences.indices.drop(1).reverse) {
      subSequences(index - 1).prepend(subSequences(index - 1).head - subSequences(index).head)
    }
    subSequences.head.head
  }


  override def parseInput(raw: List[String]): Array[Array[Int]] =
    raw.filter(_.nonEmpty).map(_.split(" ").filter(_.nonEmpty).map(_.toInt)).toArray

  override def subDir: String = "nine"
}
