package com.siril.advent.tasks.twelve

import com.siril.advent.tasks.Task

import scala.collection.mutable

object Springs extends Task[Array[Line], Long] {

  override def solve(input: Array[Line]): Long = {
    val solved = input.map { g =>
      cache.clear()
      solveLineDP(g.statuses, g.groups)
    }
    solved.sum
  }

  type Key = (Int, Int, Int)
  private val cache: mutable.Map[Key, Long] = mutable.Map.empty

  private def solveLineDP(statuses: Array[Char], groups: Array[Int]): Long = {
    // if false branch needs to be pruned
    def isStillValid(gIndex: Int, currenGroupSize: Int): Boolean = gIndex < groups.length && groups(gIndex) == currenGroupSize

    def processDot(index: Int, gIndex: Int, currenGroupSize: Int): Long =
      if (currenGroupSize > 0 && isStillValid(gIndex, currenGroupSize))
        calculate(index + 1, gIndex + 1, 0)
      else if (currenGroupSize == 0) calculate(index + 1, gIndex, 0) else 0

    def processHash(index: Int, gIndex: Int, currenGroupSize: Int): Long =
      calculate(index + 1, gIndex, currenGroupSize + 1)

    def calculate(index: Int, gIndex: Int, currenGroupSize: Int): Long = {
      val key = (index, gIndex, currenGroupSize)
      if (cache.contains(key))
        cache(key)
      else if (index == statuses.length) {
        if (gIndex >= groups.length && currenGroupSize == 0) 1 //ex: "#.##. 1,2" on last '.' gIndex will be == groups.length
        else if (gIndex == groups.length - 1 && groups(gIndex) == currenGroupSize) 1 // #.# 1,1 on lat '#' gIndex = 1 currenGroupSize = 1
        else 0
      } else {
        val result =
          if (statuses(index) == '?')
            processDot(index, gIndex, currenGroupSize) + processHash(index, gIndex, currenGroupSize)
          else if (statuses(index) == '.')
            processDot(index, gIndex, currenGroupSize)
          else if (statuses(index) == '#')
            processHash(index, gIndex, currenGroupSize)
          else 0
        cache.put(key, result)
        result
      }
    }

    calculate(0, 0, 0)
  }


  def solveLineBruteForce(group: Line): Long = {
    def isValid(statuses: Array[Char], sequence: Array[Int]): Boolean = {
      var curr = 0
      var sequenceIndex: Int = -1
      var isValid: Option[Boolean] = None
      for (i <- statuses.indices if isValid.isEmpty) {
        if (sequenceIndex > sequence.length - 1) {
          isValid = Some(false)
        } else if (statuses(i) == '.' && curr > 0) {
          sequenceIndex = sequenceIndex + 1
          if (sequenceIndex > sequence.length - 1 || sequence(sequenceIndex) != curr)
            isValid = Some(false)
          curr = 0
        } else if (statuses(i) == '#') {
          curr = curr + 1
          if (i == statuses.length - 1 && curr > 0) {
            sequenceIndex = sequenceIndex + 1
            if (sequenceIndex > sequence.length - 1 || sequence(sequenceIndex) != curr)
              isValid = Some(false)
          }
        }
      }
      if (sequenceIndex != sequence.length - 1)
        isValid = Some(false)

      val res = isValid.getOrElse(true)
      res
    }

    def f(statuses: Array[Char], sequence: Array[Int], i: Int): Long = {
      if (i == statuses.length) {
        if (isValid(statuses, sequence)) 1 else 0
      }
      else if (statuses(i) == '?') {
        val one = (statuses.take(i) :+ '#') ++ statuses.drop(i + 1)
        val two = (statuses.take(i) :+ '.') ++ statuses.drop(i + 1)
        f(one, sequence, i + 1) + f(two, sequence, i + 1)
      } else
        f(statuses, sequence, i + 1)
    }

    f(group.statuses, group.groups, 0)
  }

  override def solveAdvanced(input: Array[Line]): Long = {
    val multiplied = input.map(g => Line(
      (0 until 5).map(_ => g.statuses.mkString).mkString("?").toCharArray,
      (0 until 5).flatMap(_ => g.groups).toArray)
    )

    val solved = multiplied.map { g =>
      cache.clear()
      solveLineDP(g.statuses, g.groups)
    }
    solved.sum
  }

  override def parseInput(raw: List[String]): Array[Line] = {
    raw.filter(_.nonEmpty).map(
      _.split(" ").toList match {
        case List(a, b) => Line(a.toCharArray, b.split(",").map(_.toInt))
      }
    ).toArray
  }

  override def subDir: String = "twelve"
}

case class Line(statuses: Array[Char], groups: Array[Int])
