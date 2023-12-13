package com.siril.advent.tasks.thirteen

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class MirrorsTest extends Specification {
  sequential
  "Mirrors" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(405)
    }

    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(28895)
    }

    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(400)
    }

    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(31603)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Array[Char]]], Long] {
    override def instance: Mirrors.type = Mirrors
  }
}
