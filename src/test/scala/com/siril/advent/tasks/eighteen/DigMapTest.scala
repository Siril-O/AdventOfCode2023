package com.siril.advent.tasks.eighteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class DigMapTest extends Specification {
  "DigMap" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(62)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(34329)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(952408144115L)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(42617947302920L)
    }
  }

  trait Ctx extends TestCtx[Array[DigItem], Long] {
    override def instance: DigMap.type = DigMap
  }

}
