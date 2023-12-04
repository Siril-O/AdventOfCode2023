package com.siril.advent.tasks.one

import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope
import org.specs2.mutable._

class TrebuchetTest extends Specification {

  "Trebuchet" should {

    "calculate test set without letters" in new Ctx {
      trebuchet.apply(testInput) must beEqualTo(142)
    }
    "calculate with test set with letters" in new Ctx {
      trebuchetWithLetters.apply(InputReader.readLines("one/testInputLetters.txt")) must beEqualTo(281)
    }

    "calculate A set without letters" in new Ctx {
      trebuchet.apply(InputReader.readLines("one/inputA.txt")) must beEqualTo(54159)
    }
    "calculate A set set with letters" in new Ctx {
      trebuchetWithLetters.apply(InputReader.readLines("one/inputA.txt")) must beEqualTo(53866)
    }
    "test" in new Ctx {
      trebuchetWithLetters.processLines(List("sixthree6lxcrsevenseven69twonegs")) must beEqualTo(List(61))
    }
  }

  trait Ctx extends Scope {

    val trebuchet = new Trebuchet(false)
    val trebuchetWithLetters = new Trebuchet(true)

    val testInput = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    )
  }
}

