package com.siril.advent.tasks.one

import com.siril.advent.tasks.TestCtx
import com.siril.advent.utils.InputReader
import org.specs2.mutable._

class TrebuchetTest extends Specification {

  "Trebuchet" should {
    "solve test" in new Ctx {
      instance.solve(testInput) must beEqualTo(142)
    }
    "solve task" in new Ctx {
      instance.solve(taskInput) must beEqualTo(54159)
    }
    "solve advanced test" in new Ctx {
      instance.solveAdvanced(testInputLetters) must beEqualTo(281)
    }
    "solve advanced task" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(53866)
    }
  }

  trait Ctx extends TestCtx[List[String], Int] {
    override def instance: Trebuchet.type = Trebuchet

    val testInputLetters = InputReader.readLines("testInputLetters.txt", instance)
  }
}

