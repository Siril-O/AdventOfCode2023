package com.siril.advent.tasks.eleven

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class GalaxiesTest extends Specification {
  "Galaxies" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(374)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(10292708)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(82000210)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(790194712336L)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Char]], Long] {
    override def instance: Galaxies.type = Galaxies
  }
}

