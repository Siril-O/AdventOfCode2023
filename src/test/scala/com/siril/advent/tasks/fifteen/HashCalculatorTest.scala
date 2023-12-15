package com.siril.advent.tasks.fifteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class HashCalculatorTest extends Specification {
  "HashCalculator" should {
    "solve test input" in new Ctx {
      instance.solve(Array("HASH")) must beEqualTo(52)

      instance.solve(testInput) must beEqualTo(1320)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(514281)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(145)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(244199)
    }
  }

  trait Ctx extends TestCtx[Array[String], Long] {
    override def instance: HashCalculator.type = HashCalculator
  }
}
