package com.siril.advent.tasks.six

import com.siril.advent.tasks.{Task, TestCtx}
import org.specs2.mutable.Specification

class RaceTest extends Specification {

  "Race" should {
    "calculate test set" in new Ctx {
      instance.solve(testInput) must beEqualTo(288)
    }
    "calculate A set" in new Ctx {
      instance.solve(taskInput) must beEqualTo(303600)
    }
    "calculate seed ranges test set" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(71503)
    }
    "calculate seed ranges A set" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(23654842)
    }
  }

  trait Ctx extends TestCtx[Map[Long, Long], Long] {
    override def instance: Race.type = Race
  }
}