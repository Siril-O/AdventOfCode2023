package com.siril.advent.tasks.fourteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class BouldersTest extends Specification {
  "Boulders" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(136)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(106990)
    }

    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(64)
    }

    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(100531)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Char]], Long] {
    override def instance: Boulders.type = Boulders
  }
}
