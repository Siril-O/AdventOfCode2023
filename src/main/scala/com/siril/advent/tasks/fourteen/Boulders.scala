package com.siril.advent.tasks.fourteen

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


object Boulders extends Task[Array[Array[Char]], Long] {

  private val boulderSymbol = 'O'
  override def solve(input: Array[Array[Char]]): Long = {
    val boulders: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer(input.map(ArrayBuffer(_: _*)): _*)
    boulders.head.indices.foreach(moveBouldersUp(boulders, _))
    callBouldersWeight(boulders)
  }

  override def solveAdvanced(input: Array[Array[Char]]): Long = {
    val boulders: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer(input.map(ArrayBuffer(_: _*)): _*)
    val cycles = 1000000000
    val sequenceToFindLcs = 500
    val weights = new ArrayBuffer[Int]()
    (0 until sequenceToFindLcs).foreach { _ =>
      moveCycle(boulders)
      weights.addOne(callBouldersWeight(boulders))
    }

    val (start, end) = findFirstRepeatableSubSequence(weights.toArray).get
    val sequence = weights.slice(start, end)
    val seqIndex = ((cycles - start) % (end - start)) - 1
    sequence(seqIndex)
  }

  private def findFirstRepeatableSubSequence(arr: Array[Int], maxFrame: Int = 50): Option[(Int, Int)] = {
    var result = Option.empty[(Int, Int)]
    var found = false
    for (i <- arr.indices if !found) {
      for (frame <- (2 to maxFrame) if !found) {
        for (j <- (0 to (arr.length - 1) / frame) if !found) {
          val aPos = (j + i) * frame
          val sliceA = arr.slice(aPos, aPos + frame)
          val sliceB = arr.slice(aPos + frame, aPos + frame + frame)
          if (sliceA sameElements sliceB) {
            found = true
            result = Some((aPos, aPos + frame))
          }
        }
      }
    }
    result
  }
  private def callBouldersWeight(boulders: ArrayBuffer[ArrayBuffer[Char]]): Int =
    boulders.zip((1 to boulders.length).reverse).map(v =>
      v._1.map(c => if (c == boulderSymbol) v._2 else 0)
    ).map(_.sum).sum

  private def moveCycle(boulders: ArrayBuffer[ArrayBuffer[Char]]): Unit = {
    boulders.head.indices.foreach(moveBouldersUp(boulders, _))
    boulders.foreach(moveBouldersLeft)
    boulders.head.indices.foreach(moveBouldersDown(boulders, _))
    boulders.foreach(moveBouldersRight)
  }

  private def moveBouldersUp(input: ArrayBuffer[ArrayBuffer[Char]], colIndex: Int): Unit = {
    var leftStopPosition = 0
    for {i <- input.indices} {
      val symbol = input(i)(colIndex)
      if (symbol == '#') {
        leftStopPosition = i + 1
      } else if (symbol == boulderSymbol) {
        if (leftStopPosition == i)
          leftStopPosition = leftStopPosition + 1
        else {
          swap(input, leftStopPosition -> colIndex, i -> colIndex)
          leftStopPosition = leftStopPosition + 1
        }
      }
    }
  }

  private def moveBouldersDown(input: ArrayBuffer[ArrayBuffer[Char]], colIndex: Int): Unit = {
    var leftStopPosition = input.length - 1
    for {i <- input.indices.reverse} {
      val symbol = input(i)(colIndex)
      if (symbol == '#') {
        leftStopPosition = i - 1
      } else if (symbol == boulderSymbol) {
        if (leftStopPosition == i)
          leftStopPosition = leftStopPosition - 1
        else {
          swap(input, leftStopPosition -> colIndex, i -> colIndex)
          leftStopPosition = leftStopPosition - 1
        }
      }
    }
  }

  private def moveBouldersLeft(line: ArrayBuffer[Char]): Unit = {
    var leftStopPosition = 0
    for {i <- line.indices}
      if (line(i) == '#') {
        leftStopPosition = i + 1
      } else if (line(i) == boulderSymbol) {
        if (leftStopPosition == i)
          leftStopPosition = leftStopPosition + 1
        else {
          swap(line, leftStopPosition, i)
          leftStopPosition = leftStopPosition + 1
        }
      }
  }

  private def moveBouldersRight(line: ArrayBuffer[Char]): Unit = {
    var leftStopPosition = line.length - 1
    for {i <- line.indices.reverse}
      if (line(i) == '#') {
        leftStopPosition = i - 1
      } else if (line(i) == boulderSymbol) {
        if (leftStopPosition == i)
          leftStopPosition = leftStopPosition - 1
        else {
          swap(line, leftStopPosition, i)
          leftStopPosition = leftStopPosition - 1
        }
      }
    line.toArray
  }

  private def swap[T: ClassTag](arr: ArrayBuffer[T], aIndex: Int, bIndex: Int): Unit = {
    val tmp = arr(aIndex)
    arr.update(aIndex, arr(bIndex))
    arr.update(bIndex, tmp)
  }

  private def swap[T: ClassTag](arr: ArrayBuffer[ArrayBuffer[T]], aCoord: (Int, Int), bCoord: (Int, Int)): Unit = {
    val (ax, ay) = aCoord
    val (bx, by) = bCoord
    if (ax == bx) {
      swap(arr(ax), ay, by)
    } else {
      val tmp = arr(ax)(ay)
      arr(ax).update(ay, arr(bx)(by))
      arr(bx).update(by, tmp)
    }
  }


  override def parseInput(raw: List[String]): Array[Array[Char]] =
    raw.map(_.toCharArray).toArray

  override def subDir: String = "fourteen"
}
