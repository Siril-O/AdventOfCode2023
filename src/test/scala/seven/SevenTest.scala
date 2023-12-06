package seven

import com.siril.advent.tasks.TestCtx
import com.siril.advent.tasks.seven.Seven
import com.siril.advent.tasks.seven.SevenF.{Input, Output}
import org.specs2.mutable.Specification

class SevenTest extends Specification {

  "Seven" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(35)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(173706076)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(46)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(11611182)
    }
  }

  trait Ctx extends TestCtx[Input, Output] {
    override def instance: Seven.type = Seven
  }
}
