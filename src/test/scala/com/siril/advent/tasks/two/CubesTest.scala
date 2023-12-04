package com.siril.advent.tasks.two

import com.siril.advent.tasks.two.Colors.{Blue, Green, Red}
import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope
import org.specs2.mutable._

class CubesTest extends Specification {

  "Cubes" should {
    "calculate test set" in new Ctx {
      cubes.apply(InputReader.readTaskTwo("test.txt")) must beEqualTo(8)
    }
    "calculate inputA set" in new Ctx {
      cubes.apply(InputReader.readTaskTwo("inputA.txt")) must beEqualTo(2406)
    }
    "maxByColors test set"in new Ctx {
      cubes.maxByColors(InputReader.readTaskTwo("test.txt")) must beEqualTo(2286)
    }
    "maxByColors inputA set" in new Ctx {
      cubes.maxByColors(InputReader.readTaskTwo("inputA.txt")) must beEqualTo(78375)
    }
  }

  trait Ctx extends Scope {
    val cubes = new Cubes(Map(Red -> 12, Green -> 13, Blue -> 14))
  }
}
