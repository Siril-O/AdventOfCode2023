package com.siril.advent.tasks.one

import com.siril.advent.tasks.Task


/**
 * You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").
 *
 * As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.
 *
 * The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
 *
 * For example:
 *
 * 1abc2
 * pqr3stu8vwx
 * a1b2c3d4e5f
 * treb7uchet
 * In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
 *
 * Consider your entire calibration document. What is the sum of all of the calibration values?
 */
class Trebuchet(includeWrittenDigits: Boolean) extends Task[List[String], Int] {

  private val digits = "1234567890".toCharArray.toSet

  private val letterRegexp = "(one|two|three|four|five|six|seven|eight|nine|[0-9])".r

  private val reverseLetterRegexp = "(enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|[0-9])".r


  override def apply(lines: List[String]): Int =
    processLines(lines).sum

  def processLines(lines: List[String]): List[Int] =
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
}
