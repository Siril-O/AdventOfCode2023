package com.siril.advent.tasks.three

import com.siril.advent.utils.InputReader
import org.specs2.mutable._

class EngineFixerTest extends Specification {

  "EngineFixer" should {

    "calculate test set" in {
      EngineFixer(InputReader.readLines("test.txt", EngineFixer)) must beEqualTo(4361)
    }

    "calculate  setA" in {
      EngineFixer(InputReader.readLines("inputA.txt", EngineFixer)) must beEqualTo(509115)
    }

    "calculate gears test set" in {
      EngineFixer.calculateGearRatio(InputReader.readLines("test.txt", EngineFixer)) must beEqualTo(467835)
    }

    "calculate gears setA" in {
      EngineFixer.calculateGearRatio(InputReader.readLines("inputA.txt", EngineFixer)) must beEqualTo(75220503)
    }
  }
}
