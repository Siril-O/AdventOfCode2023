package com.siril.advent.tasks.two

import com.siril.advent.tasks.TestCtx
import com.siril.advent.tasks.two.Colors.{Blue, Green, Red}
import org.specs2.mutable._

class CubesTest extends Specification {

  "Cubes" should {
    "run test set" in new Ctx {
      instance.solve(testInput) must beEqualTo(8)
    }
    "run inputA set" in new Ctx {
      instance.solve(taskInput) must beEqualTo(2406)
    }
    "run test set" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(2286)
    }
    "run inputA set" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(78375)
    }
  }

  trait Ctx extends TestCtx[List[Game], Int] {
    override def instance: Cubes = new Cubes(Map(Red -> 12, Green -> 13, Blue -> 14))
  }
}
