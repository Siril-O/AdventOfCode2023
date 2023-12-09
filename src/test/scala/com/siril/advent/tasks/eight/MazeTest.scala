package com.siril.advent.tasks.eight

import com.siril.advent.tasks.TestCtx
import com.siril.advent.tasks.eigth.{Maze, MazeNavigator, Utils}
import com.siril.advent.utils.InputReader
import org.specs2.mutable.Specification

class MazeTest extends Specification {

  "Maze" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(2)
    }
    "solve A test input" in new Ctx {
      instance.solve(testA) must beEqualTo(6)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(18157)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testB) must beEqualTo(6)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(14299763833181L)
    }

    "Utils" in new Ctx {
      Utils.calcPrimeFactors(6) must beEqualTo(Map(2 -> 1, 3 -> 1))
      Utils.calcPrimeFactors(18) must beEqualTo(Map(2 -> 1, 3 -> 2))

      Utils.calcLCMFast(Array(48, 18)) must beEqualTo(48 * 18 / 6)

      Utils.calcLCM(Array(12, 18)) must beEqualTo(36)
      Utils.calcLCM(Array(48, 18, 56)) must beEqualTo(1008)
      Utils.calcLCMFast(Array(48, 18, 56)) must beEqualTo(1008)
    }
  }

  trait Ctx extends TestCtx[Maze, Long] {
    override def instance: MazeNavigator.type = MazeNavigator

    val testA = InputReader.readLines("testA.txt", instance)
    val testB = InputReader.readLines("testB.txt", instance)
  }
}
