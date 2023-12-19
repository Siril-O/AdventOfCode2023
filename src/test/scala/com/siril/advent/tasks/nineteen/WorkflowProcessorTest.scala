package com.siril.advent.tasks.nineteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class WorkflowProcessorTest extends Specification {
  "WorkflowProcessor" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(19114)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(575412)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(167409079868000L)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(126107942006821L)
    }
  }

  trait Ctx extends TestCtx[Workflows, Long] {
    override def instance: WorkflowProcessor.type = WorkflowProcessor
  }

}
