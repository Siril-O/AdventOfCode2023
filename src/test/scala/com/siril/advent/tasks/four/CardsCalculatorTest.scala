package com.siril.advent.tasks.four

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class CardsCalculatorTest extends Specification {

  "CardsCalculator" should {
    "calculate test set" in new Ctx {
      instance.solve(testInput) must beEqualTo(13)
    }
    "calculate A set" in new Ctx {
      instance.solve(taskInput) must beEqualTo(28538)
    }
    "calculate test set with new rules" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(9425061)
    }
  }

  trait Ctx extends TestCtx[Array[Card], Int] {
    override def instance: CardsCalculator.type = CardsCalculator
  }
}
