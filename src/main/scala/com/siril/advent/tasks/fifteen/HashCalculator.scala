package com.siril.advent.tasks.fifteen

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

object HashCalculator extends Task[Array[String], Long] {
  override def solve(input: Array[String]): Long = {
    input.map(calcHash).sum
  }

  private def calcHash(str: String): Int = {
    var result = 0
    str.toCharArray.foreach { c =>
      result = result + c.toInt
      result = result * 17
      result = result % 256
    }
    result
  }

  private val reg = "([a-zA-Z]+)\\s*([=\\-])\\s*(\\d*)".r

  override def solveAdvanced(input: Array[String]): Long = {
    val hashMap = ArrayBuffer[ArrayBuffer[(String, Int)]]((0 until 256).map(_ => new ArrayBuffer[(String, Int)]()): _*)
    input.foreach { i =>
      val (label, insert, focusStrength) = i match {
        case reg(label, operation, focusStrength) => (label, operation == "=", if (focusStrength.isEmpty) None else Some(focusStrength.toInt))
      }
      val boxIndex = calcHash(label)
      val bucket = hashMap(boxIndex)
      val elementIndex = bucket.zipWithIndex.collectFirst { case ((l, _), i) if l == label => i }
      (label, insert, focusStrength) match {
        case (label, true, Some(focusStrength)) if elementIndex.isDefined => bucket.update(elementIndex.get, label -> focusStrength)
        case (label, true, Some(focusStrength)) => bucket.addOne(label -> focusStrength)
        case (_, false, None) if elementIndex.isDefined => bucket.remove(elementIndex.get)
        case _ => ()
      }
    }
    val processed = hashMap.zipWithIndex.filter(_._1.nonEmpty).flatMap {
      case (b, bi) => b.zipWithIndex.map { case ((_, fl), i) =>
        val result = (1L + bi) * (i + 1) * fl
        result
      }
    }
    processed.sum
  }


  override def parseInput(raw: List[String]): Array[String] = raw.mkString.split(",")

  override def subDir: String = "fifteen"
}
