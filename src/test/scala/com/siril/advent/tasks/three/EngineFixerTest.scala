package com.siril.advent.tasks.three

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable._

class EngineFixerTest extends Specification {

  "EngineFixer" should {
    "calculate test set" in new Ctx {
      instance.solve(testInput) must beEqualTo(4361)
    }
    "calculate setA" in new Ctx {
      instance.solve(taskInput) must beEqualTo(509115)
    }
    "calculate gears test set" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(467835)
    }
    "calculate gears setA" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(75220503)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Char]], Int] {
    override def instance: EngineFixer.type = EngineFixer
  }
}
