package com.siril.advent.tasks.five

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class AlmanacProcessorTest extends Specification {

  "AlmanacProcessor" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(35)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(173706076)
    }
    "solve advanced test" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(46)
    }
    "solve advanced task" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(11611182)
    }
  }

  trait Ctx extends TestCtx[Almanac, Long] {
    override def instance: AlmanacProcessor.type = AlmanacProcessor
  }
}
