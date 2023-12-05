package com.siril.advent.tasks.four

import com.siril.advent.utils.InputReader
import org.specs2.mutable.Specification

class CardsCalculatorTest extends Specification {

  "CardsCalculator" should {
    "calculate test set" in {
      CardsCalculator.apply(InputReader.readLines("test.txt", CardsCalculator)) must beEqualTo(13)
    }
    "calculate A set" in {
      CardsCalculator.apply(InputReader.readLines("inputA.txt", CardsCalculator)) must beEqualTo(28538)
    }
    "calculate test set with new rules" in {
      CardsCalculator.calcIncludingCopies(InputReader.readLines("inputA.txt", CardsCalculator)) must beEqualTo(9425061)
    }
  }
}
