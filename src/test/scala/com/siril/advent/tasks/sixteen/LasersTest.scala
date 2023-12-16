package com.siril.advent.tasks.sixteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class LasersTest extends Specification {
  sequential
  "Lasers" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(46)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(8389)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(51)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(8564)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Char]], Long] {
    override def instance: Lasers.type = Lasers
  }
}
