package com.siril.advent.tasks.ten

import com.siril.advent.tasks.TestCtx
import com.siril.advent.utils.InputReader
import org.specs2.mutable.Specification

class PipesTest extends Specification {


  "Pipes" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(4)
    }
    "solve test A input" in new Ctx {
      instance.solve(inputA) must beEqualTo(8)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(6599)
    }
    "solve advanced A test input" in new Ctx {
      instance.solveAdvanced(advInputA) must beEqualTo(4)
    }
    "solve advanced B test input" in new Ctx {
      instance.solveAdvanced(advInputB) must beEqualTo(8)
    }
    "solve advanced C test input" in new Ctx {
      instance.solveAdvanced(advInputC) must beEqualTo(10)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(477)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Char]], Long] {
    override def instance: Pipes.type = Pipes

    val inputA: Array[Array[Char]] = InputReader.readLines("testA.txt", instance)
    val advInputA: Array[Array[Char]] = InputReader.readLines("advancedTestInputA.txt", instance)
    val advInputB: Array[Array[Char]] = InputReader.readLines("advancedTestInputB.txt", instance)
    val advInputC: Array[Array[Char]] = InputReader.readLines("advancedTestInputC.txt", instance)

  }
}
