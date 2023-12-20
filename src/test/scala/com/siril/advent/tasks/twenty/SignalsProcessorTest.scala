package com.siril.advent.tasks.twenty

import com.siril.advent.tasks.TestCtx
import com.siril.advent.utils.InputReader
import org.specs2.mutable.Specification

class SignalsProcessorTest extends Specification {
  "SignalsProcessor" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(32000000)
    }
    "solve testB input" in new Ctx {
      instance.solve(testBInput) must beEqualTo(11687500)
    }

    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(898557000)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(238420328103151L)
    }
  }

  trait Ctx extends TestCtx[Map[String, Element], Long] {
    override def instance: SignalsProcessor.type = SignalsProcessor
    val testBInput = InputReader.readLines("testB.txt", instance)

  }

}
