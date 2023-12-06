package com.siril.advent.tasks.one

import com.siril.advent.tasks.Task

object Trebuchet extends Task[List[String], Int] {

  private val digits = "1234567890".toCharArray.toSet

  private val letterRegexp = "(one|two|three|four|five|six|seven|eight|nine|[0-9])".r

  private val reverseLetterRegexp = "(enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|[0-9])".r


  override def solve(lines: List[String]): Int =
    processLines(lines, includeWrittenDigits = false).sum

  override def solveAdvanced(lines: List[String]): Int =
    processLines(lines, includeWrittenDigits = true).sum

  def processLines(lines: List[String], includeWrittenDigits: Boolean): List[Int] =
    lines.map { line =>
      if (includeWrittenDigits) {
        val first = letterRegexp.findFirstIn(line).get
        val rev = reverseLetterRegexp.findFirstIn(line.reverse).get
        s"${letterToDigit(first)}${letterToDigit(rev.reverse)}".toInt
      } else {
        s"${line.find(digits).get}${line.findLast(digits).get}".toInt
      }
    }

  private def letterToDigit(str: String): Int = str match {
    case "one" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case "five" => 5
    case "six" => 6
    case "seven" => 7
    case "eight" => 8
    case "nine" => 9
    case rest => rest.toInt
  }

  override def parseInput(raw: List[String]): List[String] = raw

  override def subDir: String = "one"
}
