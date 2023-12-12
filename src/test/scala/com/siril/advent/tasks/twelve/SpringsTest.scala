package com.siril.advent.tasks.twelve

import com.siril.advent.tasks.TestCtx
import org.specs2.mutable.Specification

class SpringsTest extends Specification {
  sequential
  "Springs" should {
    "solve test input" in new Ctx {
      //      instance.isValid("#.#.###".toCharArray, Array(1,1,3)) must beTrue
      //      instance.isValid("#.#.###.".toCharArray, Array(1,1,3)) must beTrue
      //      instance.isValid("..#..##...###.".toCharArray, Array(1,2, 3)) must beTrue
      //      instance.isValid(".#.###.#.######".toCharArray, Array(1,3,1,6)) must beTrue
      //      instance.isValid("####.#...#...".toCharArray, Array(4,1,1)) must beTrue
      //      instance.isValid("..#".toCharArray, Array(1)) must beTrue
      //      instance.isValid(".###.##.#.##".toCharArray, Array(3,2,1)) must beFalse

      instance.solve(Array(Line("?#?#?#?#?#?#?#?".toCharArray, Array(1, 3, 1, 6)))) must beEqualTo(1)
      instance.solve(testInput) must beEqualTo(21)

      testInput.map(instance.solveLineBruteForce).sum  must beEqualTo(21)
    }

    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(7047)
    }

    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(525152)
    }

    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(17391848518844L)
    }
  }

  trait Ctx extends TestCtx[Array[Line], Long] {
    override def instance: Springs.type = Springs
  }
}
