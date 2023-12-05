package com.siril.advent.tasks.five

import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope
import org.specs2.mutable.Specification

class AlmanacProcessorTest extends Specification {

  "AlmanacProcessor" should {
    "calculate test set" in new Ctx {
      instance(testSet) must beEqualTo(35)
    }
    "calculate A set" in new Ctx {
      instance(setA) must beEqualTo(173706076)
    }
    "calculate seed ranges test set" in new Ctx {
      instance.calcSeedRanges(testSet) must beEqualTo(46)
    }
    "calculate seed ranges A set" in new Ctx {
      instance.calcSeedRanges(setA) must beEqualTo(11611182)
    }
  }

  trait Ctx extends Scope {
    val instance = AlmanacProcessor

    val testSet: Almanac = InputReader.readLines("test.txt", instance)
    val setA = InputReader.readLines("inputA.txt", instance)
  }
}
