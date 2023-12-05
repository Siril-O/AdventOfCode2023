package com.siril.advent.tasks.one

import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope
import org.specs2.mutable._

class TrebuchetTest extends Specification {

  "Trebuchet" should {

    "calculate test set without letters" in new Ctx {
      Trebuchet(testInput) must beEqualTo(142)
    }
    "calculate with test set with letters" in new Ctx {
      Trebuchet.applyWithWrittenDigits(InputReader.readLines("testInputLetters.txt", Trebuchet)) must beEqualTo(281)
    }

    "calculate A set without letters" in new Ctx {
      Trebuchet(InputReader.readLines("inputA.txt", Trebuchet)) must beEqualTo(54159)
    }
    "calculate A set set with letters" in new Ctx {
      Trebuchet.applyWithWrittenDigits(InputReader.readLines("inputA.txt", Trebuchet)) must beEqualTo(53866)
    }
    "test" in new Ctx {
      Trebuchet.processLines(List("sixthree6lxcrsevenseven69twonegs"), includeWrittenDigits = true) must beEqualTo(List(61))
    }
  }

  trait Ctx extends Scope {

    val testInput = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    )
  }
}

