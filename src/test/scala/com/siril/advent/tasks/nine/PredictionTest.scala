package com.siril.advent.tasks.nine

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class PredictionTest extends Specification {

  "Prediction" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(114)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(1938800261)
    }

    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(2)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(1112)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Int]], Long] {
    override def instance: Prediction.type = Prediction
  }
}
