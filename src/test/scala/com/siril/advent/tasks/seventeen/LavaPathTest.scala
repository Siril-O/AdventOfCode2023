package com.siril.advent.tasks.seventeen

import com.siril.advent.tasks.TestCtx
import com.siril.advent.tasks.seventeen.LavaPath.Graph
import org.specs2.mutable.Specification

class LavaPathTest extends Specification {
  sequential
  "LavaPath" should {
    "solve test input" in new Ctx {
      val g = new Graph(testInput)
      g.resolveNextPosition(0, 0, Right, 1) must beEqualTo(Array(((0, 1), Right, 2), ((1, 0), Down, 1)))
      g.resolveNextPosition(testInput.length - 1, 0, Down, 1) must beEqualTo(Array(((testInput.length - 1, 1), Right, 1)))
      g.resolveNextPosition(0, 0, Right, 2) must beEqualTo(Array(((0, 1), Right, 3), ((1, 0), Down, 1)))
      g.resolveNextPosition(0, 0, Right, 3) must beEqualTo(Array(((1, 0), Down, 1)))

      new Graph(testInput).dijkstraWithMoves()  must beEqualTo(102)
//      println()
//      instance.solve(testInput)// must beEqualTo(102)
//      new Graph(testInput).yetNewDijkstra() must beEqualTo(102)
//      new Graph(testInput).yetYetNewDijkstra() must beEqualTo(102)
    }
    "solve task input" in new Ctx {
      new Graph(taskInput).dijkstraWithMoves()// must beEqualTo(1128)
//      instance.solve(taskInput)// must beEqualTo(1128)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(51)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(8564)
    }
  }

  trait Ctx extends TestCtx[Array[Array[Int]], Int] {
    override def instance: LavaPath.type = LavaPath
  }
}

