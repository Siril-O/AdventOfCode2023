package com.siril.advent.tasks.three

import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope
import org.specs2.mutable._

class EngineFixerTest extends Specification {

  "EngineFixer" should {

    "calculate test set" in new Ctx {
      engine(InputReader.readTaskThree("test.txt")) must beEqualTo(4361)
    }

    "calculate  setA" in new Ctx {
      engine(InputReader.readTaskThree("inputA.txt")) must beEqualTo(509115)
    }

    "calculate gears test set" in new Ctx {
      engine.calculateGearRatio(InputReader.readTaskThree("test.txt")) must beEqualTo(467835)
    }

    "calculate gears setA" in new Ctx {
      engine.calculateGearRatio(InputReader.readTaskThree("inputA.txt")) must beEqualTo(75220503)
    }
  }

  trait Ctx extends Scope {

    val engine = new EngineFixer()
  }

}
